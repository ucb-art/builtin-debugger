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

    io.status.state ?== PatternGeneratorState.Idle.id
    io.control.bits.readyBypass <<= readyBypass
    io.control.bits.triggerMode <<= triggerMode.id
    io.control.bits.lastSample <<= numSamples - 1
    io.control.bits.continuous <<= continuous
    io.control.bits.arm <<= 1
    io.control.bits.abort <<= 0
    io.control.valid <<= 1
    step()
    io.control.valid <<= 0
    io.status.state ?== PatternGeneratorState.Armed.id
  }

  /** Writes the buffer using the memory interface, returns the number of samples written.
    *
    */
  def writeBuffer(io: PatternGenerator#PatternGeneratorIO, contents: Seq[Seq[BigInt]])(implicit t: InnerTester): Int = {
    var numSamples: Int = 0

    for ((line, i) <- contents.zipWithIndex) {
      io.status.state ?== PatternGeneratorState.Idle.id
      io.memory.ready ?== 1
      io.memory.bits.writeAddr <<= i
      io.memory.valid <<= 1
      for ((data, j) <- line.zipWithIndex) {
        io.memory.bits.writeData(j) <<= data
        numSamples += 1
      }
      step()
    }
    io.memory.valid <<= 0

    numSamples
  }

  def generatorStep(io: PatternGenerator#PatternGeneratorIO, expectedState: PatternGeneratorStateType, expectedSignal: Option[Int],
      expectedSampled: SampleState, ready: Boolean = true, trigger: TriggerState = TInvalid,
      expectedOverflow: Boolean = false)(implicit t: InnerTester) {
    io.status.state ?== expectedState.id

    trigger match {
      case THigh | TLow => io.trigger.valid <<= 1
      case TInvalid | TInvHigh | TInvLow => io.trigger.valid <<= 0
    }
    trigger match {
      case THigh | TInvalid | TInvHigh => io.trigger.bits <<= 1
      case TLow | TInvLow => io.trigger.bits <<= 0
    }

    expectedSignal match {
      case Some(expectedSignal) => {
        io.signal.valid ?== true
        io.signal.bits ?== expectedSignal
      }
      case None => io.signal.valid ?== false
    }
    expectedSampled match {
      case Started(sample) => {
        io.status.numSampled ?== sample
        io.status.started ?== true
        io.status.overflow ?== false
      }
      case Overflow(sample) => {
        io.status.numSampled ?== sample
        io.status.started ?== true
        io.status.overflow ?== true
      }
      case NotStarted => {
        io.status.started ?== false
        io.status.overflow ?== false
      }
    }

    io.signal.ready <<= ready

    step()
  }
}

class PatternGeneratorInterfaceSpec extends FlatSpec with PatternGeneratorImplicitTester {
  "PatternGenerator StreaminigMemoryQueue" should "work" in {
    run(new Module {
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
      c.io.pgIo.control.valid <<= false
      c.io.saqReset <<= false

      // Stream data through
      c.io.saqInput.valid <<= true

      c.io.saqInput.bits(0) <<= 1
      c.io.saqAddr ?== 0
      c.io.saqInput.ready ?== true
      step()

      c.io.saqInput.bits(0) <<= 2
      c.io.saqAddr ?== 1
      c.io.saqInput.ready ?== true
      step()

      c.io.saqInput.bits(0) <<= 3
      c.io.saqAddr ?== 2
      c.io.saqInput.ready ?== true
      step()

      c.io.saqInput.bits(0) <<= 4
      c.io.saqAddr ?== 3
      c.io.saqInput.ready ?== true
      step()

      c.io.saqAddr ?== 0  // wraparound behavior
      c.io.saqInput.valid <<= false

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
