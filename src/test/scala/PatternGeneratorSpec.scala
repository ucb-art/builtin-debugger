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
    for ((line, i) <- contents.zipWithIndex) {
      expect(c.io.status.state, PatternGeneratorState.Idle.id, "can't set pattern generator buffer unless in idle")
      expect(c.io.memory.ready, 1, "pattern generator memory interface not ready")
      poke(c.io.memory.bits.writeAddr, i)
      poke(c.io.memory.valid, 1)
      for ((data, j) <- line.zipWithIndex) {
        poke(c.io.memory.bits.writeData(j), data)
      }
      step(1)
    }
    poke(c.io.memory.valid, 0)
  
    expect(c.io.status.state, PatternGeneratorState.Idle.id, "can't arm pattern generator unless in idle")
    poke(c.io.control.bits.readyBypass, readyBypass)
    poke(c.io.control.bits.triggerMode, triggerMode.id)
    poke(c.io.control.bits.lastSample, contents.length - 1)
    poke(c.io.control.bits.arm, 1)
    poke(c.io.control.bits.abort, 0)
    poke(c.io.control.valid, 1)
    step(1)
    poke(c.io.control.valid, 0)
    expect(c.io.status.state, PatternGeneratorState.Armed.id, "pattern generator did not arm")
  }

  def generatorStep(expectedState: PatternGeneratorStateType, expectedSignal: Option[Int], expectedSampled: Int, expectedValid: Boolean,
      ready: Boolean, trigger: Boolean, expectedOverflow: Boolean = false) {
    expect(c.io.status.state, expectedState.id, "state mismatch")
    expectedSignal match {
      case Some(expectedSignal) => expect(c.io.signal.data, expectedSignal)
      case None =>
    }
    expect(c.io.signal.valid, expectedValid)
    expect(c.io.status.numSampled, expectedSampled, "sampled mismatch")
    expect(c.io.status.overflow, expectedOverflow, "overflow mismatch")
    poke(c.io.signal.ready, ready)
    poke(c.io.signal.trigger, trigger)
    step(1)
  }
}
