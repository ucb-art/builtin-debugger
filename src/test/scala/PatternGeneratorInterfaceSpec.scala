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
      val smq = pg.createStreamingMemoryInterface()

      val io = IO(new Bundle {
        val pgIo = pg.io.cloneType

        val smqReset = smq.io.reset.chiselCloneType
        val smqAddr = smq.io.addr.chiselCloneType
        val smqInput = smq.io.input.chiselCloneType
      })

      // Don't connect the memory line, leave that for the interface to generate
      io.pgIo.signal <> pg.io.signal
      io.pgIo.trigger <> pg.io.trigger
      io.pgIo.control <> pg.io.control
      io.pgIo.status <> pg.io.status

      io.smqReset <> smq.io.reset
      io.smqAddr <> smq.io.addr
      io.smqInput <> smq.io.input
    }) {implicit t => c =>
      c.io.smqReset <<= false

      // Stream data through
      c.io.smqInput.valid <<= true

      c.io.smqInput.bits(0) <<= 1
      c.io.smqAddr ?== 0
      c.io.smqInput.ready ?== true
      step()

      c.io.smqInput.bits(0) <<= 2
      c.io.smqAddr ?== 1
      c.io.smqInput.ready ?== true
      step()

      c.io.smqInput.bits(0) <<= 3
      c.io.smqAddr ?== 2
      c.io.smqInput.ready ?== true
      step()

      c.io.smqInput.bits(0) <<= 4
      c.io.smqAddr ?== 3
      c.io.smqInput.ready ?== true
      step()

      c.io.smqAddr ?== 0  // wraparound behavior
      c.io.smqInput.valid <<= false

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
