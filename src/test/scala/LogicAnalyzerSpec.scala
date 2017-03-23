// See LICENSE for license details.

package debuggers.test

import Chisel.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import debuggers._

object AnalyzerState extends Enumeration {  // TODO: DRYify
  type AnalyzerStateType = Value
  val Idle, Armed, Running = Value
}
import AnalyzerState.AnalyzerStateType

trait LogicAnalyzerTestUtils extends PeekPokeTester[LogicAnalyzer] {
  def c: LogicAnalyzer

  /** Reads out the logic analyzer buffer through the memory interface port.
    * Logic analyzer must be in the idle state. Steps through cycles.
    * expectedContents may be partial (need not contain all lines or all contents of any line).
    */
  def readCompare(expectedContents: Seq[Seq[BigInt]]) {
    for ((line, i) <- expectedContents.zipWithIndex) {
      expect(c.io.status.state, AnalyzerState.Idle.id, "can't read logic analyzer unless in idle")
      poke(c.io.memory.reqAddr, i)
      step(1)
      for ((data, j) <- line.zipWithIndex) {
        expect(c.io.memory.respData(j), data, s"data mismatch, expected $data in line $i position $j, in vector $expectedContents")
      }
    }
  }

  def arm(validBypass: Boolean, triggerMode: TriggerBlock.Mode, numSamples: Int) {
    expect(c.io.status.state, AnalyzerState.Idle.id, "can't arm logic analyzer unless in idle")
    poke(c.io.control.bits.validBypass, validBypass)
    poke(c.io.control.bits.triggerMode, triggerMode)
    poke(c.io.control.bits.numSamples, numSamples)
    poke(c.io.control.bits.arm, 1)
    poke(c.io.control.bits.abort, 0)
    poke(c.io.control.valid, 1)
    step(1)
    poke(c.io.control.valid, 0)
  }

  def analyzerStep(expectedState: AnalyzerStateType, expectedSampled: Int,
      signal: Int, valid: Boolean = true, trigger: TriggerState = TInvalid, expectedOverflow: Boolean = false) {
    expect(c.io.status.state, expectedState.id, "state mismatch")
    expect(c.io.status.numSampled, expectedSampled, "sampled mismatch")
    expect(c.io.status.overflow, expectedOverflow, "overflow mismatch")

    poke(c.io.signal.bits, signal)
    poke(c.io.signal.valid, valid)

    trigger match {
      case THigh | TLow => poke(c.io.trigger.valid, 1)
      case TInvalid | TInvHigh | TInvLow => poke(c.io.trigger.valid, 0)
    }
    trigger match {
      case THigh | TInvalid | TInvHigh => poke(c.io.trigger.bits, 1)
      case TLow | TInvLow => poke(c.io.trigger.bits, 0)
    }

    step(1)
  }
}

class LogicAnalyzerTester(val c: LogicAnalyzer) extends PeekPokeTester(c) with LogicAnalyzerTestUtils {
  // Very very basic test
  arm(true, TriggerBlock.Always, 4)
  analyzerStep(AnalyzerState.Armed, 0, 0)
  analyzerStep(AnalyzerState.Running, 1, 2)
  analyzerStep(AnalyzerState.Running, 2, 3)
  analyzerStep(AnalyzerState.Running, 3, 6)
  analyzerStep(AnalyzerState.Idle, 4, 0)
  readCompare(List(
      List(0),
      List(2),
      List(3),
      List(6)
  ))

  // Test with low numSampled
  arm(true, TriggerBlock.Always, 2)
  analyzerStep(AnalyzerState.Armed, 0, 7)
  analyzerStep(AnalyzerState.Running, 1, 42)
  analyzerStep(AnalyzerState.Idle, 2, 0)
  readCompare(List(
      List(7),
      List(42),
      List(3),  // these should not have changed
      List(6)
  ))

  // Test with valid
  arm(false, TriggerBlock.Always, 3)
  analyzerStep(AnalyzerState.Armed, 0, 8)
  analyzerStep(AnalyzerState.Running, 1, 0, valid=false)
  analyzerStep(AnalyzerState.Running, 1, 0, valid=false)  // test invalid
  analyzerStep(AnalyzerState.Running, 1, 16)
  analyzerStep(AnalyzerState.Running, 2, 31)  // test back-to-back valid
  analyzerStep(AnalyzerState.Idle, 3, 0)
  readCompare(List(
      List(8),
      List(16),
      List(31)
  ))

  // Test continuous mode
  arm(true, TriggerBlock.Always, 0)
  analyzerStep(AnalyzerState.Armed, 0, 1)
  analyzerStep(AnalyzerState.Running, 1, 2)
  analyzerStep(AnalyzerState.Running, 2, 3)
  analyzerStep(AnalyzerState.Running, 3, 4)
  analyzerStep(AnalyzerState.Running, 4, 5)
  analyzerStep(AnalyzerState.Running, 1, 6, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 2, 7, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 3, 8, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 4, 9, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 1, 10, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 2, 11, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 3, 12, expectedOverflow=true)  // sample on abort request cycle
  analyzerStep(AnalyzerState.Idle, 4, 0, expectedOverflow=true)
  readCompare(List(
      List(9),
      List(10),
      List(11),
      List(12)
  ))

  // Test numSample = 1 and overflow resets
  arm(true, TriggerBlock.Always, 1)
  analyzerStep(AnalyzerState.Armed, 0, 10)
  analyzerStep(AnalyzerState.Idle, 1, 0)
  readCompare(List(
      List(10)
  ))

  // Test continuous mode, abort on non-aligned buffer index
  arm(true, TriggerBlock.Always, 0)
  analyzerStep(AnalyzerState.Armed, 0, 1)
  analyzerStep(AnalyzerState.Running, 1, 2)
  analyzerStep(AnalyzerState.Running, 2, 3)
  analyzerStep(AnalyzerState.Running, 3, 4)
  analyzerStep(AnalyzerState.Running, 4, 5)
  analyzerStep(AnalyzerState.Running, 1, 6, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 2, 7, expectedOverflow=true)  // sample on abort request cycle
  analyzerStep(AnalyzerState.Idle, 3, 0, expectedOverflow=true)
  readCompare(List(
      List(5),
      List(6),
      List(7),
      List(4)
  ))

  // Mid-run abort in non-continuous mode
  arm(true, TriggerBlock.Always, 4)
  analyzerStep(AnalyzerState.Armed, 0, 1)
  analyzerStep(AnalyzerState.Running, 1, 2)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 2, 3)
  analyzerStep(AnalyzerState.Idle, 3, 0)
  readCompare(List(
      List(1),
      List(2),
      List(3),
      List(4)  // previous value not overwritten
  ))

  // Pre-trigger abort
  arm(true, TriggerBlock.High, 4)
  analyzerStep(AnalyzerState.Armed, 0, 0)
  analyzerStep(AnalyzerState.Armed, 0, 0)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Armed, 0, 0)
  analyzerStep(AnalyzerState.Idle, 0, 0)
}

class LogicAnalyzerCombinationalTriggerTester(val c: LogicAnalyzer) extends PeekPokeTester(c) with LogicAnalyzerTestUtils {
  // Test high trigger
  arm(true, TriggerBlock.High, 2)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvLow)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvHigh)  // invalid trigger discarded
  analyzerStep(AnalyzerState.Armed, 0, 2, trigger=THigh)
  analyzerStep(AnalyzerState.Running, 1, 4)
  analyzerStep(AnalyzerState.Idle, 2, 0)
  readCompare(List(
      List(2),
      List(4)
  ))

  // Test valid is independent of trigger
  arm(false, TriggerBlock.High, 2)
  analyzerStep(AnalyzerState.Armed, 0, 0, valid=false, trigger=TInvHigh)  // invalid trigger discarded
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvHigh)  // valid data before trigger
  analyzerStep(AnalyzerState.Armed, 0, 0, valid=false, trigger=THigh)  // trigger w/o sampling
  analyzerStep(AnalyzerState.Running, 0, 7)
  analyzerStep(AnalyzerState.Running, 1, 0, valid=false, trigger=THigh)
  analyzerStep(AnalyzerState.Running, 1, 14)
  analyzerStep(AnalyzerState.Idle, 2, 0)
  readCompare(List(
      List(7),
      List(14)
  ))

  // Test low trigger
  arm(true, TriggerBlock.Low, 2)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvHigh)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvLow)  // invalid trigger discarded
  analyzerStep(AnalyzerState.Armed, 0, 2, trigger=TLow)
  analyzerStep(AnalyzerState.Running, 1, 4)
  analyzerStep(AnalyzerState.Idle, 2, 0)
  readCompare(List(
      List(2),
      List(4)
  ))

  // Test rising edge trigger, starting low
  arm(true, TriggerBlock.Rising, 2)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvLow)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvHigh)  // discard invalid edge
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=THigh)  // discard invalid edge
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TLow)
  analyzerStep(AnalyzerState.Armed, 0, 2, trigger=THigh)
  analyzerStep(AnalyzerState.Running, 1, 4)
  analyzerStep(AnalyzerState.Idle, 2, 0)
  readCompare(List(
      List(2),
      List(4)
  ))

  // Test falling edge trigger, starting high
  arm(true, TriggerBlock.Falling, 2)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvHigh)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvLow)  // discard invalid edge
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TLow)  // discard invalid edge
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=THigh)
  analyzerStep(AnalyzerState.Armed, 0, 3, trigger=TLow)
  analyzerStep(AnalyzerState.Running, 1, 6)
  analyzerStep(AnalyzerState.Idle, 2, 0)
  readCompare(List(
      List(3),
      List(6)
  ))

  // Test rising edge trigger, valid-gated
  arm(false, TriggerBlock.Rising, 2)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvLow)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvHigh)  // dischard invalid rising edge
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=THigh)  // dischard invalid rising edge
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TLow)
  analyzerStep(AnalyzerState.Armed, 0, 2, valid=false, trigger=THigh)  // trigger w/o sampling
  analyzerStep(AnalyzerState.Running, 0, 2)
  analyzerStep(AnalyzerState.Running, 1, 0, valid=false)
  analyzerStep(AnalyzerState.Running, 1, 4)
  analyzerStep(AnalyzerState.Idle, 2, 0)
  readCompare(List(
      List(2),
      List(4)
  ))
}

class LogicAnalyzerNoncombinationalTriggerTester(val c: LogicAnalyzer) extends PeekPokeTester(c) with LogicAnalyzerTestUtils {
  // Test high trigger
  arm(true, TriggerBlock.High, 2)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvLow)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvHigh)  // invalid trigger discarded
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=THigh)  // no sampling on trigger
  analyzerStep(AnalyzerState.Running, 0, 2)
  analyzerStep(AnalyzerState.Running, 1, 4)
  analyzerStep(AnalyzerState.Idle, 2, 0)
  readCompare(List(
      List(2),
      List(4)
  ))

  // Test valid is independent of trigger
  arm(false, TriggerBlock.High, 2)
  analyzerStep(AnalyzerState.Armed, 0, 0, valid=false, trigger=TInvHigh)  // invalid trigger discarded
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvHigh)  // valid data before trigger
  analyzerStep(AnalyzerState.Armed, 0, 0, valid=false, trigger=THigh)  // trigger w/o sampling
  analyzerStep(AnalyzerState.Running, 0, 7)
  analyzerStep(AnalyzerState.Running, 1, 0, valid=false, trigger=THigh)
  analyzerStep(AnalyzerState.Running, 1, 14)
  analyzerStep(AnalyzerState.Idle, 2, 0)
  readCompare(List(
      List(7),
      List(14)
  ))

  // Test rising edge trigger, starting low
  arm(true, TriggerBlock.Rising, 2)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvLow)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvHigh)  // discard invalid edge
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=THigh)  // discard invalid edge
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TLow)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=THigh)  // no sampling on trigger
  analyzerStep(AnalyzerState.Running, 0, 2)
  analyzerStep(AnalyzerState.Running, 1, 4)
  analyzerStep(AnalyzerState.Idle, 2, 0)
  readCompare(List(
      List(2),
      List(4)
  ))

  // Test rising edge trigger, valid-gated
  arm(false, TriggerBlock.Rising, 2)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvLow)
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TInvHigh)  // dischard invalid rising edge
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=THigh)  // dischard invalid rising edge
  analyzerStep(AnalyzerState.Armed, 0, 0, trigger=TLow)
  analyzerStep(AnalyzerState.Armed, 0, 2, valid=false, trigger=THigh)  // trigger w/o sampling
  analyzerStep(AnalyzerState.Running, 0, 2)
  analyzerStep(AnalyzerState.Running, 1, 0, valid=false)
  analyzerStep(AnalyzerState.Running, 1, 4)
  analyzerStep(AnalyzerState.Idle, 2, 0)
  readCompare(List(
      List(2),
      List(4)
  ))
}

class LogicAnalyzerNonalignedDepthTester(val c: LogicAnalyzer) extends PeekPokeTester(c) with LogicAnalyzerTestUtils {
  // Full depth behavior
  arm(true, TriggerBlock.Always, 5)
  analyzerStep(AnalyzerState.Armed, 0, 1)
  analyzerStep(AnalyzerState.Running, 1, 2)
  analyzerStep(AnalyzerState.Running, 2, 3)
  analyzerStep(AnalyzerState.Running, 3, 4)
  analyzerStep(AnalyzerState.Running, 4, 5)
  analyzerStep(AnalyzerState.Idle, 5, 0)
  readCompare(List(
      List(1),
      List(2),
      List(3),
      List(4),
      List(5)
  ))

  // Partial depth behavior
  arm(true, TriggerBlock.Always, 4)
  analyzerStep(AnalyzerState.Armed, 0, 10)
  analyzerStep(AnalyzerState.Running, 1, 11)
  analyzerStep(AnalyzerState.Running, 2, 12)
  analyzerStep(AnalyzerState.Running, 3, 13)
  analyzerStep(AnalyzerState.Idle, 4, 0)
  readCompare(List(
      List(10),
      List(11),
      List(12),
      List(13),
      List(5)  // should not be overwritten
  ))

  // Continuous mode behavior
  arm(true, TriggerBlock.Always, 0)
  analyzerStep(AnalyzerState.Armed, 0, 1)
  analyzerStep(AnalyzerState.Running, 1, 2)
  analyzerStep(AnalyzerState.Running, 2, 3)
  analyzerStep(AnalyzerState.Running, 3, 4)
  analyzerStep(AnalyzerState.Running, 4, 5)

  analyzerStep(AnalyzerState.Running, 5, 6)
  analyzerStep(AnalyzerState.Running, 1, 7, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 2, 8, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 3, 9, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 4, 10, expectedOverflow=true)  // sample on abort request cycle

  analyzerStep(AnalyzerState.Idle, 5, 0, expectedOverflow=true)
  readCompare(List(
      List(6),
      List(7),
      List(8),
      List(9),
      List(10)
  ))
}

class LogicAnalyzerMultilineTester(val c: LogicAnalyzer) extends PeekPokeTester(c) with LogicAnalyzerTestUtils {
  // Full depth behavior
  arm(true, TriggerBlock.Always, 4)
  analyzerStep(AnalyzerState.Armed, 0, 1)
  analyzerStep(AnalyzerState.Running, 1, 2)
  analyzerStep(AnalyzerState.Running, 2, 3)
  analyzerStep(AnalyzerState.Running, 3, 4)
  analyzerStep(AnalyzerState.Idle, 4, 0)
  readCompare(List(
      List(1, 2),
      List(3, 4)
  ))

  // Partial depth behavior
  arm(true, TriggerBlock.Always, 3)
  analyzerStep(AnalyzerState.Armed, 0, 10)
  analyzerStep(AnalyzerState.Running, 1, 11)
  analyzerStep(AnalyzerState.Running, 2, 12)
  analyzerStep(AnalyzerState.Idle, 3, 0)
  readCompare(List(
      List(10, 11),
      List(12, 4)  // partial line should not be overwritten
  ))

  // Continuous mode full depth
  arm(true, TriggerBlock.Always, 0)
  analyzerStep(AnalyzerState.Armed, 0, 1)
  analyzerStep(AnalyzerState.Running, 1, 2)
  analyzerStep(AnalyzerState.Running, 2, 3)
  analyzerStep(AnalyzerState.Running, 3, 4)

  analyzerStep(AnalyzerState.Running, 4, 14)
  analyzerStep(AnalyzerState.Running, 1, 15, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 2, 16, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 3, 17, expectedOverflow=true)  // sample on abort request cycle

  analyzerStep(AnalyzerState.Idle, 4, 0, expectedOverflow=true)
  readCompare(List(
      List(14, 15),
      List(16, 17)
  ))

  // Continuous mode partial depth
  arm(true, TriggerBlock.Always, 0)
  analyzerStep(AnalyzerState.Armed, 0, 1)
  analyzerStep(AnalyzerState.Running, 1, 2)
  analyzerStep(AnalyzerState.Running, 2, 3)
  analyzerStep(AnalyzerState.Running, 3, 4)

  analyzerStep(AnalyzerState.Running, 4, 5)
  analyzerStep(AnalyzerState.Running, 1, 6, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 2, 7, expectedOverflow=true)  // sample on abort request cycle

  analyzerStep(AnalyzerState.Idle, 3, 0, expectedOverflow=true)
  readCompare(List(
      List(5, 6),
      List(7, 4)
  ))
}

class LogicAnalyzerSpec extends ChiselFlatSpec {
  "LogicAnalyzer basics" should "work" in {
    Driver(() => new LogicAnalyzer(8, 1, 4)) {
      c => new LogicAnalyzerTester(c)
    } should be (true)
  }
  "LogicAnalyzer combinational trigger" should "work" in {
    Driver(() => new LogicAnalyzer(8, 1, 4)) {
      c => new LogicAnalyzerCombinationalTriggerTester(c)
    } should be (true)
  }
  "LogicAnalyzer noncombinational trigger" should "work" in {
    Driver(() => new LogicAnalyzer(8, 1, 4, false)) {
      c => new LogicAnalyzerNoncombinationalTriggerTester(c)
    } should be (true)
  }
  "LogicAnalyzer with non-power-of-two samples" should "work" in {
    Driver(() => new LogicAnalyzer(8, 1, 5)) {
      c => new LogicAnalyzerNonalignedDepthTester(c)
    } should be (true)
  }
  "LogicAnalyzer with multiple signals per line" should "work" in {
    Driver(() => new LogicAnalyzer(8, 2, 4)) {
      c => new LogicAnalyzerMultilineTester(c)
    } should be (true)
  }
}
