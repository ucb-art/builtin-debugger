// See LICENSE for license details.

package debuggers.test

import Chisel.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import debuggers._

// TODO: dedup with LASpec
import TriggerMode.TriggerModeType

// TODO: dedup with LASpec
object PatternGeneratorState extends Enumeration {  // TODO: DRYify
  type PatternGeneratorStateType = Value
  val Idle, Armed, Running = Value
}
import PatternGeneratorState.PatternGeneratorStateType

trait PatternGeneratorTestUtils extends PeekPokeTester[PatternGenerator] {
  def c: PatternGenerator

  /** Sets the pattern generator's pattern buffer, then arms it.
    * Number of samples is set to the length of `contents`.
    */
  def arm(readyBypass: Boolean, triggerMode: TriggerModeType, contents: Seq[Seq[BigInt]], continuous: Boolean) {
    var numSamples: Int = 0

    poke(c.io.control.valid, 0)
    for ((line, i) <- contents.zipWithIndex) {
      expect(c.io.status.state, PatternGeneratorState.Idle.id, "can't set pattern generator buffer unless in idle")
      expect(c.io.memory.ready, 1, "pattern generator memory interface not ready")
      poke(c.io.memory.bits.writeAddr, i)
      poke(c.io.memory.valid, 1)
      for ((data, j) <- line.zipWithIndex) {
        poke(c.io.memory.bits.writeData(j), data)
        numSamples += 1
      }
      step(1)
    }
    poke(c.io.memory.valid, 0)

    expect(c.io.status.state, PatternGeneratorState.Idle.id, "can't arm pattern generator unless in idle")
    poke(c.io.control.bits.readyBypass, readyBypass)
    poke(c.io.control.bits.triggerMode, triggerMode.id)
    poke(c.io.control.bits.lastSample, numSamples - 1)
    poke(c.io.control.bits.continuous, continuous)
    poke(c.io.control.bits.arm, 1)
    poke(c.io.control.bits.abort, 0)
    poke(c.io.control.valid, 1)
    step(1)
    poke(c.io.control.valid, 0)
    expect(c.io.status.state, PatternGeneratorState.Armed.id, "pattern generator did not arm")
  }

  def generatorStep(expectedState: PatternGeneratorStateType, expectedSignal: Option[Int],
      expectedSampled: Int, expectedValid: Boolean, ready: Boolean, trigger: Boolean,
      expectedOverflow: Boolean = false) {
    expect(c.io.status.state, expectedState.id, "state mismatch")
    System.out.println(peek(c.io.status.state))
    expectedSignal match {
      case Some(expectedSignal) => expect(c.io.signal.data, expectedSignal)
      case None =>
    }
    expect(c.io.signal.valid, expectedValid, "valid mismatch")
    System.out.println(peek(c.io.signal.valid))
    expect(c.io.status.numSampled, expectedSampled, "sampled mismatch")
    System.out.println(peek(c.io.status.numSampled))
    expect(c.io.status.overflow, expectedOverflow, "overflow mismatch")
    System.out.println(peek(c.io.status.overflow))
    poke(c.io.signal.ready, ready)
    poke(c.io.signal.trigger, trigger)
    step(1)
  }
}

class PatternGeneratorTester(val c: PatternGenerator) extends PeekPokeTester(c) with PatternGeneratorTestUtils {
  // Very very basic test
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0, false, true, true)
  generatorStep(PatternGeneratorState.Running, Some(1), 1, true, true, true)
  generatorStep(PatternGeneratorState.Running, Some(2), 2, true, true, true)
  generatorStep(PatternGeneratorState.Running, Some(3), 3, true, true, true)
  generatorStep(PatternGeneratorState.Running, Some(4), 4, true, true, true)
  generatorStep(PatternGeneratorState.Idle, None, 4, false, true, true)

}

class PatternGeneratorSpec extends ChiselFlatSpec {
  "Simple PatternGenerator" should "work" in {
    Driver(() => new PatternGenerator(8, 1, 4)) {
      c => new PatternGeneratorTester(c)
    } should be (true)
  }
}
