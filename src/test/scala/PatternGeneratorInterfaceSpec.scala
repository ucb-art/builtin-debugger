// See LICENSE for license details.

package debuggers.test

import org.scalatest._

import chisel3._
import chisel3.iotesters.experimental.ImplicitPokeTester

import debuggers._

import TriggerMode.TriggerModeType
import PatternGeneratorState.PatternGeneratorStateType

trait PatternGeneratorImplicitTester extends ImplicitPokeTester {
  /** Sets the pattern generator's pattern buffer, then arms it.
    * Number of samples is set to the length of `contents`.
    */
  def arm(io: PatternGenerator#PatternGeneratorIO, readyBypass: Boolean, triggerMode: TriggerModeType,
      numSamples: Int, continuous: Boolean)(implicit t: InnerTester) {
    require(numSamples > 0)

    check(io.status.state, PatternGeneratorState.Idle.id)
    poke(io.control.bits.readyBypass, readyBypass)
    poke(io.control.bits.triggerMode, triggerMode.id)
    poke(io.control.bits.lastSample, numSamples - 1)
    poke(io.control.bits.continuous, continuous)
    poke(io.control.bits.arm, 1)
    poke(io.control.bits.abort, 0)
    poke(io.control.valid, 1)
    step()
    poke(io.control.valid, 0)
    check(io.status.state, PatternGeneratorState.Armed.id)
  }

  /** Writes the buffer using the memory interface, returns the number of samples written.
    *
    */
  def writeBuffer(io: PatternGenerator#PatternGeneratorIO, contents: Seq[Seq[BigInt]])(implicit t: InnerTester): Int = {
    var numSamples: Int = 0

    for ((line, i) <- contents.zipWithIndex) {
      check(io.status.state, PatternGeneratorState.Idle.id)
      check(io.memory.ready, 1)
      poke(io.memory.bits.writeAddr, i)
      poke(io.memory.valid, 1)
      for ((data, j) <- line.zipWithIndex) {
        poke(io.memory.bits.writeData(j), data)
        numSamples += 1
      }
      step()
    }
    poke(io.memory.valid, 0)

    numSamples
  }

  def generatorStep(io: PatternGenerator#PatternGeneratorIO, expectedState: PatternGeneratorStateType, expectedSignal: Option[Int],
      expectedSampled: SampleState, ready: Boolean = true, trigger: TriggerState = TInvalid,
      expectedOverflow: Boolean = false)(implicit t: InnerTester) {
    check(io.status.state, expectedState.id)

    trigger match {
      case THigh | TLow => poke(io.trigger.valid, 1)
      case TInvalid | TInvHigh | TInvLow => poke(io.trigger.valid, 0)
    }
    trigger match {
      case THigh | TInvalid | TInvHigh => poke(io.trigger.bits, 1)
      case TLow | TInvLow => poke(io.trigger.bits, 0)
    }

    expectedSignal match {
      case Some(expectedSignal) => {
        check(io.signal.valid, true)
        check(io.signal.bits, expectedSignal)
      }
      case None => check(io.signal.valid, false)
    }
    expectedSampled match {
      case Started(sample) => {
        check(io.status.numSampled, sample)
        check(io.status.started, true)
        check(io.status.overflow, false)
      }
      case Overflow(sample) => {
        check(io.status.numSampled, sample)
        check(io.status.started, true)
        check(io.status.overflow, true)
      }
      case NotStarted => {
        check(io.status.started, false)
        check(io.status.overflow, false)
      }
    }

    poke(io.signal.ready, ready)

    step()
  }
}

class PatternGeneratorInterfaceSpec extends FlatSpec with PatternGeneratorImplicitTester {
  "PatternGenerator StreaminigMemoryQueue" should "work" in {
    test(new Module {
      val pg = Module(new PatternGenerator(8, 1, 4, false))
      val saq = Module(new StreamingAddressQueue(pg.io.memory.bits.writeData.cloneType, pg.memDepth))

      val io = IO(new Bundle {
        val pgIo = pg.io.chiselCloneType

        val saqReset = saq.io.reset.chiselCloneType
        val saqInput = saq.io.input.chiselCloneType

        val saqAddr = Output(saq.io.addr.cloneType)
      })

      pg.io.memory.valid := saq.io.output.valid
      saq.io.output.ready := pg.io.memory.ready
      pg.io.memory.bits.writeAddr := saq.io.addr
      pg.io.memory.bits.writeData := saq.io.output.bits

      io.pgIo.signal <> pg.io.signal
      io.pgIo.trigger <> pg.io.trigger
      io.pgIo.control <> pg.io.control
      io.pgIo.status <> pg.io.status

      io.saqReset <> saq.io.reset
      io.saqInput <> saq.io.input

      io.saqAddr <> saq.io.addr
    }) {implicit t => c =>
      poke(c.io.pgIo.control.valid, false)
      poke(c.io.saqReset, false)

      // Stream data through
      poke(c.io.saqInput.valid, true)
      poke(c.io.saqInput.bits(0), 1)
      check(c.io.saqAddr, 0)
      check(c.io.saqInput.ready, true)
      step()

      poke(c.io.saqInput.bits(0), 2)
      check(c.io.saqAddr, 1)
      check(c.io.saqInput.ready, true)
      step()

      poke(c.io.saqInput.bits(0), 3)
      check(c.io.saqAddr, 2)
      check(c.io.saqInput.ready, true)
      step()

      poke(c.io.saqInput.bits(0), 4)
      check(c.io.saqAddr, 3)
      check(c.io.saqInput.ready, true)
      step()

      check(c.io.saqAddr, 0)  // wraparound behavior
      poke(c.io.saqInput.valid, false)

      // Test read-out
      arm(c.io.pgIo, true, TriggerMode.None, 4, false)
      generatorStep(c.io.pgIo, PatternGeneratorState.Armed, None, NotStarted)
      generatorStep(c.io.pgIo, PatternGeneratorState.Running, Some(1), 0)
      generatorStep(c.io.pgIo, PatternGeneratorState.Running, Some(2), 1)
      generatorStep(c.io.pgIo, PatternGeneratorState.Running, Some(3), 2)
      generatorStep(c.io.pgIo, PatternGeneratorState.Running, Some(4), 3)
      generatorStep(c.io.pgIo, PatternGeneratorState.Idle, None, Overflow(0))
    }
  }
}
