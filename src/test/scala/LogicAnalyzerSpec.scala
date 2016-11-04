// See LICENSE for license details.

package debuggers.test

import Chisel.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import debuggers._

object TriggerMode extends Enumeration {  // TODO: DRYify
  val None, High, Low, Rising, Falling = Value
}

object AnalyzerState extends Enumeration {  // TODO: DRYify
  val Idle, Armed, Running = Value
}

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

  def arm(validBypass: Boolean, triggerMode: Int, numSamples: Int) {
    expect(c.io.status.state, AnalyzerState.Idle.id)
    poke(c.io.control.bits.validBypass, validBypass)
    poke(c.io.control.bits.triggerMode, triggerMode)
    poke(c.io.control.bits.numSamples, numSamples)
    poke(c.io.control.bits.arm, 1)
    poke(c.io.control.bits.abort, 0)
    poke(c.io.control.valid, 1)
    step(1)
    poke(c.io.control.valid, 0)
    expect(c.io.status.state, AnalyzerState.Armed.id)
  }

}

class LogicAnalyzerTester(val c: LogicAnalyzer) extends PeekPokeTester(c) with LogicAnalyzerTestUtils {
  arm(true, TriggerMode.None.id, 4)
  expect(c.io.status.numSampled, 0)
  poke(c.io.signal.data, 0)
  step(1)
  expect(c.io.status.state, AnalyzerState.Running.id)
  expect(c.io.status.numSampled, 1)
  poke(c.io.signal.data, 2)
  step(1)
  expect(c.io.status.state, AnalyzerState.Running.id)
  expect(c.io.status.numSampled, 2)
  poke(c.io.signal.data, 3)
  step(1)
  expect(c.io.status.state, AnalyzerState.Running.id)
  expect(c.io.status.numSampled, 3)
  poke(c.io.signal.data, 6)
  step(1)
  expect(c.io.status.state, AnalyzerState.Idle.id)
  expect(c.io.status.numSampled, 4)

  step(10)

  readCompare(List(
      List(0),
      List(2),
      List(3),
      List(6)
  ))
}

class GCDTester extends ChiselFlatSpec {
  "Simple one sample/line LogicAnalyzer" should "work" in {
    Driver(() => new LogicAnalyzer(8, 1, 4)) {
      c => new LogicAnalyzerTester(c)
    } should be (true)
  }
}
