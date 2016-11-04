// See LICENSE for license details.

package debuggers.test

import Chisel.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import debuggers._

object TriggerMode extends Enumeration {  // TODO: DRYify
  type TriggerModeType = Value
  val None, High, Low, Rising, Falling = Value
}
import TriggerMode.TriggerModeType

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
      expect(c.io.status.state, AnalyzerState.Idle.id)
      poke(c.io.memory.reqAddr, i)
      step(1)
      for ((data, j) <- line.zipWithIndex) {
        expect(c.io.memory.respData(j), data)
      }
    }
  }

  def arm(validBypass: Boolean, triggerMode: TriggerModeType, numSamples: Int) {
    expect(c.io.status.state, AnalyzerState.Idle.id)
    poke(c.io.control.bits.validBypass, validBypass)
    poke(c.io.control.bits.triggerMode, triggerMode.id)
    poke(c.io.control.bits.numSamples, numSamples)
    poke(c.io.control.bits.arm, 1)
    poke(c.io.control.bits.abort, 0)
    poke(c.io.control.valid, 1)
    step(1)
    poke(c.io.control.valid, 0)
    expect(c.io.status.state, AnalyzerState.Armed.id)
  }

  def analyzerStep(expectedState: AnalyzerStateType, expectedSampled: Int, valid: Boolean,
      trigger: Boolean, signal: Int) {
    expect(c.io.status.state, expectedState.id)
    expect(c.io.status.numSampled, expectedSampled)
    poke(c.io.signal.valid, valid)
    poke(c.io.signal.trigger, trigger)
    poke(c.io.signal.data, signal)
    step(1)
  }
}

class LogicAnalyzerTester(val c: LogicAnalyzer) extends PeekPokeTester(c) with LogicAnalyzerTestUtils {
  // Basic test without valid or trigger
  arm(true, TriggerMode.None, 4)
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
  arm(true, TriggerMode.None, 2)
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
  arm(false, TriggerMode.None, 3)
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

}

class GCDTester extends ChiselFlatSpec {
  "Simple one sample/line LogicAnalyzer" should "work" in {
    Driver(() => new LogicAnalyzer(8, 1, 4)) {
      c => new LogicAnalyzerTester(c)
    } should be (true)
  }
}
