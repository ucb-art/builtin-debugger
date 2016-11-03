// See LICENSE for license details.

package debuggers.test

import Chisel.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import debuggers._

class LogicAnalyzerTester(c: LogicAnalyzer) extends PeekPokeTester(c) {

}

class GCDTester extends ChiselFlatSpec {
  "LogicAnalyzer" should "work" in {
    Driver(() => new LogicAnalyzer(8, 8, 8)) {
      c => new LogicAnalyzerTester(c)
    } should be (true)
  }
}
