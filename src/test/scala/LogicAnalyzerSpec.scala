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
    expect(c.io.status.state, AnalyzerState.Armed.id, "logic analyzer did not arm")
  }

  def analyzerStep(expectedState: AnalyzerStateType, expectedSampled: Int, valid: Boolean,
      trigger: Boolean, signal: Int, expectedOverflow: Boolean = false) {
    expect(c.io.status.state, expectedState.id, "state mismatch")
    expect(c.io.status.numSampled, expectedSampled, "sampled mismatch")
    expect(c.io.status.overflow, expectedOverflow, "overflow mismatch")
    poke(c.io.signal.valid, valid)
    poke(c.io.signal.trigger, trigger)
    poke(c.io.signal.data, signal)
    step(1)
  }
}

class LogicAnalyzerTester(val c: LogicAnalyzer) extends PeekPokeTester(c) with LogicAnalyzerTestUtils {
  // Very very basic test
  arm(true, TriggerBlock.Always, 4)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 0)
  analyzerStep(AnalyzerState.Running, 1, true, true, 2)
  analyzerStep(AnalyzerState.Running, 2, true, true, 3)
  analyzerStep(AnalyzerState.Running, 3, true, true, 6)
  analyzerStep(AnalyzerState.Idle, 4, true, true, 0)
  readCompare(List(
      List(0),
      List(2),
      List(3),
      List(6)
  ))

  // Test with low numSampled
  arm(true, TriggerBlock.Always, 2)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 7)
  analyzerStep(AnalyzerState.Running, 1, true, true, 42)
  analyzerStep(AnalyzerState.Idle, 2, true, true, 0)
  readCompare(List(
      List(7),
      List(42),
      List(3),  // these should not have changed
      List(6)
  ))

  // Test with valid
  arm(false, TriggerBlock.Always, 3)
  analyzerStep(AnalyzerState.Armed, 0, false, true, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 8)  // test trigger gated by Valid
  analyzerStep(AnalyzerState.Running, 1, false, true, 0)
  analyzerStep(AnalyzerState.Running, 1, false, true, 0)
  analyzerStep(AnalyzerState.Running, 1, true, true, 16)  // test invalid
  analyzerStep(AnalyzerState.Running, 2, true, true, 31)  // test back-to-back valid
  analyzerStep(AnalyzerState.Idle, 3, true, true, 0)
  readCompare(List(
      List(8),
      List(16),
      List(31)
  ))

  // Test high trigger
  arm(true, TriggerBlock.High, 2)
  analyzerStep(AnalyzerState.Armed, 0, true, false, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, false, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 2)
  analyzerStep(AnalyzerState.Running, 1, true, false, 4)
  analyzerStep(AnalyzerState.Idle, 2, true, true, 0)
  readCompare(List(
      List(2),
      List(4)
  ))

  // Test valid-gated trigger
  arm(false, TriggerBlock.High, 2)
  analyzerStep(AnalyzerState.Armed, 0, true, false, 0)
  analyzerStep(AnalyzerState.Armed, 0, false, true, 0)  // invalid trigger discarded
  analyzerStep(AnalyzerState.Armed, 0, true, true, 7)
  analyzerStep(AnalyzerState.Running, 1, false, true, 0)
  analyzerStep(AnalyzerState.Running, 1, false, false, 0)
  analyzerStep(AnalyzerState.Running, 1, true, false, 14)
  analyzerStep(AnalyzerState.Idle, 2, true, true, 0)
  readCompare(List(
      List(7),
      List(14)
  ))

  // Test low trigger
  arm(true, TriggerBlock.Low, 2)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, false, 3)
  analyzerStep(AnalyzerState.Running, 1, true, true, 6)
  analyzerStep(AnalyzerState.Idle, 2, true, true, 0)
  readCompare(List(
      List(3),
      List(6)
  ))

  // Test rising edge trigger, starting low
  arm(true, TriggerBlock.Rising, 2)
  analyzerStep(AnalyzerState.Armed, 0, true, false, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, false, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 2)
  analyzerStep(AnalyzerState.Running, 1, true, false, 4)
  analyzerStep(AnalyzerState.Idle, 2, true, true, 0)
  readCompare(List(
      List(2),
      List(4)
  ))

  // Test rising edge trigger, starting high
  arm(true, TriggerBlock.Rising, 2)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 0)  // wait for false->true before triggering
  analyzerStep(AnalyzerState.Armed, 0, true, true, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, false, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 1)
  analyzerStep(AnalyzerState.Running, 1, true, false, 5)
  analyzerStep(AnalyzerState.Idle, 2, true, true, 0)
  readCompare(List(
      List(1),
      List(5)
  ))

  // Test rising edge trigger, valid-gated
  arm(false, TriggerBlock.Rising, 2)
  analyzerStep(AnalyzerState.Armed, 0, false, false, 0)  // discard invalid low
  analyzerStep(AnalyzerState.Armed, 0, true, true, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, false, 0)  // first true low
  analyzerStep(AnalyzerState.Armed, 0, false, true, 0)  // discard invalid riding edge
  analyzerStep(AnalyzerState.Armed, 0, true, true, 2)
  analyzerStep(AnalyzerState.Running, 1, true, false, 1)
  analyzerStep(AnalyzerState.Idle, 2, true, true, 0)
  readCompare(List(
      List(2),
      List(1)
  ))

  // Test continuous mode
  arm(true, TriggerBlock.Always, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 1)
  analyzerStep(AnalyzerState.Running, 1, true, true, 2)
  analyzerStep(AnalyzerState.Running, 2, true, true, 3)
  analyzerStep(AnalyzerState.Running, 3, true, true, 4)
  analyzerStep(AnalyzerState.Running, 4, true, true, 5)
  analyzerStep(AnalyzerState.Running, 1, true, true, 6, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 2, true, true, 7, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 3, true, true, 8, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 4, true, true, 9, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 1, true, true, 10, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 2, true, true, 11, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 3, true, true, 12, expectedOverflow=true)  // sample on abort request cycle
  analyzerStep(AnalyzerState.Idle, 4, true, true, 0, expectedOverflow=true)
  readCompare(List(
      List(9),
      List(10),
      List(11),
      List(12)
  ))

  // Test numSample = 1 and overflow resets
  arm(true, TriggerBlock.Always, 1)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 10)
  analyzerStep(AnalyzerState.Idle, 1, true, true, 0)
  readCompare(List(
      List(10)
  ))

  // Test continuous mode, abort on non-aligned buffer index
  arm(true, TriggerBlock.Always, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 1)
  analyzerStep(AnalyzerState.Running, 1, true, true, 2)
  analyzerStep(AnalyzerState.Running, 2, true, true, 3)
  analyzerStep(AnalyzerState.Running, 3, true, true, 4)
  analyzerStep(AnalyzerState.Running, 4, true, true, 5)
  analyzerStep(AnalyzerState.Running, 1, true, true, 6, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 2, true, true, 7, expectedOverflow=true)  // sample on abort request cycle
  analyzerStep(AnalyzerState.Idle, 3, true, true, 0, expectedOverflow=true)
  readCompare(List(
      List(5),
      List(6),
      List(7),
      List(4)
  ))

  // Mid-run abort in non-continuous mode
  arm(true, TriggerBlock.Always, 4)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 1)
  analyzerStep(AnalyzerState.Running, 1, true, true, 2)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 2, true, true, 3)
  analyzerStep(AnalyzerState.Idle, 3, true, true, 0)
  readCompare(List(
      List(1),
      List(2),
      List(3),
      List(4)  // previous value not overwritten
  ))

  // Pre-trigger abort
  arm(true, TriggerBlock.High, 4)
  analyzerStep(AnalyzerState.Armed, 0, true, false, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, false, 0)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Armed, 0, true, false, 0)
  analyzerStep(AnalyzerState.Idle, 0, true, true, 0)
}

class LogicAnalyzerNonalignedDepthTester(val c: LogicAnalyzer) extends PeekPokeTester(c) with LogicAnalyzerTestUtils {
  // Full depth behavior
  arm(true, TriggerBlock.Always, 5)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 1)
  analyzerStep(AnalyzerState.Running, 1, true, true, 2)
  analyzerStep(AnalyzerState.Running, 2, true, true, 3)
  analyzerStep(AnalyzerState.Running, 3, true, true, 4)
  analyzerStep(AnalyzerState.Running, 4, true, true, 5)
  analyzerStep(AnalyzerState.Idle, 5, true, true, 0)
  readCompare(List(
      List(1),
      List(2),
      List(3),
      List(4),
      List(5)
  ))

  // Partial depth behavior
  arm(true, TriggerBlock.Always, 4)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 10)
  analyzerStep(AnalyzerState.Running, 1, true, true, 11)
  analyzerStep(AnalyzerState.Running, 2, true, true, 12)
  analyzerStep(AnalyzerState.Running, 3, true, true, 13)
  analyzerStep(AnalyzerState.Idle, 4, true, true, 0)
  readCompare(List(
      List(10),
      List(11),
      List(12),
      List(13),
      List(5)  // should not be overwritten
  ))

  // Continuous mode behavior
  arm(true, TriggerBlock.Always, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 1)
  analyzerStep(AnalyzerState.Running, 1, true, true, 2)
  analyzerStep(AnalyzerState.Running, 2, true, true, 3)
  analyzerStep(AnalyzerState.Running, 3, true, true, 4)
  analyzerStep(AnalyzerState.Running, 4, true, true, 5)

  analyzerStep(AnalyzerState.Running, 5, true, true, 6)
  analyzerStep(AnalyzerState.Running, 1, true, true, 7, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 2, true, true, 8, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 3, true, true, 9, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 4, true, true, 10, expectedOverflow=true)  // sample on abort request cycle

  analyzerStep(AnalyzerState.Idle, 5, true, true, 0, expectedOverflow=true)
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
  analyzerStep(AnalyzerState.Armed, 0, true, true, 1)
  analyzerStep(AnalyzerState.Running, 1, true, true, 2)
  analyzerStep(AnalyzerState.Running, 2, true, true, 3)
  analyzerStep(AnalyzerState.Running, 3, true, true, 4)
  analyzerStep(AnalyzerState.Idle, 4, true, true, 0)
  readCompare(List(
      List(1, 2),
      List(3, 4)
  ))

  // Partial depth behavior
  arm(true, TriggerBlock.Always, 3)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 10)
  analyzerStep(AnalyzerState.Running, 1, true, true, 11)
  analyzerStep(AnalyzerState.Running, 2, true, true, 12)
  analyzerStep(AnalyzerState.Idle, 3, true, true, 0)
  readCompare(List(
      List(10, 11),
      List(12, 4)  // partial line should not be overwritten
  ))

  // Continuous mode full depth
  arm(true, TriggerBlock.Always, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 1)
  analyzerStep(AnalyzerState.Running, 1, true, true, 2)
  analyzerStep(AnalyzerState.Running, 2, true, true, 3)
  analyzerStep(AnalyzerState.Running, 3, true, true, 4)

  analyzerStep(AnalyzerState.Running, 4, true, true, 14)
  analyzerStep(AnalyzerState.Running, 1, true, true, 15, expectedOverflow=true)
  analyzerStep(AnalyzerState.Running, 2, true, true, 16, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 3, true, true, 17, expectedOverflow=true)  // sample on abort request cycle

  analyzerStep(AnalyzerState.Idle, 4, true, true, 0, expectedOverflow=true)
  readCompare(List(
      List(14, 15),
      List(16, 17)
  ))

  // Continuous mode partial depth
  arm(true, TriggerBlock.Always, 0)
  analyzerStep(AnalyzerState.Armed, 0, true, true, 1)
  analyzerStep(AnalyzerState.Running, 1, true, true, 2)
  analyzerStep(AnalyzerState.Running, 2, true, true, 3)
  analyzerStep(AnalyzerState.Running, 3, true, true, 4)

  analyzerStep(AnalyzerState.Running, 4, true, true, 5)
  analyzerStep(AnalyzerState.Running, 1, true, true, 6, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  analyzerStep(AnalyzerState.Running, 2, true, true, 7, expectedOverflow=true)  // sample on abort request cycle

  analyzerStep(AnalyzerState.Idle, 3, true, true, 0, expectedOverflow=true)
  readCompare(List(
      List(5, 6),
      List(7, 4)
  ))
}

class LogicAnalyzerSpec extends ChiselFlatSpec {
  "Simple LogicAnalyzer" should "work" in {
    Driver(() => new LogicAnalyzer(8, 1, 4)) {
      c => new LogicAnalyzerTester(c)
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
