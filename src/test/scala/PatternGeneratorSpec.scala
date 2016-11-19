// See LICENSE for license details.

package debuggers.test

import Chisel.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import debuggers._

object TriggerMode extends Enumeration {  // TODO: DRYify
  type TriggerModeType = Value
  val None, High, Low, Rising, Falling = Value
}
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
      expectedSampled: Int, ready: Boolean = true,
      trigger: TriggerState = TInvalid, expectedOverflow: Boolean = false) {
    expect(c.io.status.state, expectedState.id, "state mismatch")

    trigger match {
      case THigh | TLow => poke(c.io.trigger.valid, 1)
      case TInvalid | TInvHigh | TInvLow => poke(c.io.trigger.valid, 0)
    }
    trigger match {
      case THigh | TInvalid | TInvHigh => poke(c.io.trigger.bits, 1)
      case TLow | TInvLow => poke(c.io.trigger.bits, 0)
    }

    expectedSignal match {
      case Some(expectedSignal) => {
        expect(c.io.signal.bits, expectedSignal)
        expect(c.io.signal.valid, true, "expected valid true")
      }
      case None => expect(c.io.signal.valid, false, "expected valid false")
    }
    expect(c.io.status.numSampled, expectedSampled, "sampled mismatch")
    expect(c.io.status.overflow, expectedOverflow, "overflow mismatch")
    poke(c.io.signal.ready, ready)

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
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Idle, None, 4)

  // Test with partial number of samples
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Idle, None, 3)

  // Test with 1 sample
  arm(true, TriggerMode.None,
      List(
        List(1)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Idle, None, 1)

  // Test with ready
  arm(false, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0, ready=false)  // trigger independent of ready
  generatorStep(PatternGeneratorState.Running, Some(1), 1, ready=false)  // stall
  generatorStep(PatternGeneratorState.Running, Some(1), 1, ready=false)  // stall
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(4), 4, ready=false)  // stall on last cycle
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Idle, None, 4)

  // Continuous mode test
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Running, Some(1), 1, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(2), 2, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(3), 3, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(4), 4, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Idle, None, 4, expectedOverflow=true)

  // Continuous mode test, non-aligned abort
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Running, Some(1), 1, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(2), 2, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Idle, None, 2, expectedOverflow=true)

  // Continuous mode test, partial depth
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(1), 1, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(2), 2, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(3), 3, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Idle, None, 3, expectedOverflow=true)

  // Continuous mode test, 1 sample
  arm(true, TriggerMode.None,
      List(
        List(1)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(1), 1, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(1), 1, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(1), 1, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Idle, None, 1, expectedOverflow=true)

  // Mid-run abort in non-continuous mode
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Idle, None, 2)
}

class PatternGeneratorNoncombinationalTriggerTester(val c: PatternGenerator) extends PeekPokeTester(c) with PatternGeneratorTestUtils {
  // Basic trigger test
  arm(false, TriggerMode.High,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0, trigger=TInvLow)  // disabled trigger
  generatorStep(PatternGeneratorState.Armed, None, 0, trigger=TInvHigh)  // "false" trigger
  generatorStep(PatternGeneratorState.Armed, None, 0, trigger=THigh)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2, trigger=TLow)  // ensure trigger doesn't impact anything
  generatorStep(PatternGeneratorState.Running, Some(3), 3, trigger=THigh)
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Idle, None, 4)

  // Pre-trigger abort
  arm(true, TriggerMode.High,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Armed, None, 0, trigger=THigh)
  generatorStep(PatternGeneratorState.Idle, None, 0)
}

class PatternGeneratorCombinationalTriggerTester(val c: PatternGenerator) extends PeekPokeTester(c) with PatternGeneratorTestUtils {
  // Basic trigger test
  arm(false, TriggerMode.High,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0, trigger=TInvLow)  // disabled trigger
  generatorStep(PatternGeneratorState.Armed, None, 0, trigger=TInvHigh)  // "false" trigger
  generatorStep(PatternGeneratorState.Armed, Some(1), 1, trigger=THigh)
  generatorStep(PatternGeneratorState.Running, Some(2), 2, trigger=TLow)  // ensure trigger doesn't impact anything
  generatorStep(PatternGeneratorState.Running, Some(3), 3, trigger=THigh)
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Idle, None, 4)

  // Pre-trigger abort
  arm(true, TriggerMode.High,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Idle, None, 0)
}

class PatternGeneratorNonalignedDepthTester(val c: PatternGenerator) extends PeekPokeTester(c) with PatternGeneratorTestUtils {
  // Very very basic test
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4),
        List(5)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Running, Some(5), 5)
  generatorStep(PatternGeneratorState.Idle, None, 5)

  // Test with partial number of samples
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Idle, None, 4)

  // Again, but non-power-of-two
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Idle, None, 3)

  // Continuous, full depth
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4),
        List(5)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Running, Some(5), 5)
  generatorStep(PatternGeneratorState.Running, Some(1), 1, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(2), 2, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(3), 3, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(4), 4, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(5), 5, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Idle, None, 5, expectedOverflow=true)

  // Continuous, partial depth
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Running, Some(1), 1, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(2), 2, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(3), 3, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(4), 4, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Idle, None, 4, expectedOverflow=true)
}

class PatternGeneratorMultilineTester(val c: PatternGenerator) extends PeekPokeTester(c) with PatternGeneratorTestUtils {
  // Full depth
  arm(true, TriggerMode.None,
      List(
        List(1, 2),
        List(3, 4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Idle, None, 4)

  // Partial depth
  arm(true, TriggerMode.None,
      List(
        List(1, 2),
        List(3)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Idle, None, 3)

  // Continuous, full depth
  arm(true, TriggerMode.None,
      List(
        List(1, 2),
        List(3, 4)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(4), 4)
  generatorStep(PatternGeneratorState.Running, Some(1), 1, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(2), 2, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(3), 3, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(4), 4, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Idle, None, 4, expectedOverflow=true)

  // Continuous, partial depth
  arm(true, TriggerMode.None,
      List(
        List(1, 2),
        List(3)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, 0)
  generatorStep(PatternGeneratorState.Running, Some(1), 1)
  generatorStep(PatternGeneratorState.Running, Some(2), 2)
  generatorStep(PatternGeneratorState.Running, Some(3), 3)
  generatorStep(PatternGeneratorState.Running, Some(1), 1, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Running, Some(2), 2, expectedOverflow=true)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(3), 3, expectedOverflow=true)
  generatorStep(PatternGeneratorState.Idle, None, 3, expectedOverflow=true)
}

class PatternGeneratorSpec extends ChiselFlatSpec {
  "Simple PatternGenerator" should "work" in {
    Driver(() => new PatternGenerator(8, 1, 4, false)) {
      c => new PatternGeneratorTester(c)
    } should be (true)
  }
  "PatternGenerator combinational trigger" should "work" in {
    Driver(() => new PatternGenerator(8, 1, 4)) {
      c => new PatternGeneratorCombinationalTriggerTester(c)
    } should be (true)
  }
  "PatternGenerator noncombinational trigger" should "work" in {
    Driver(() => new PatternGenerator(8, 1, 4, false)) {
      c => new PatternGeneratorNoncombinationalTriggerTester(c)
    } should be (true)
  }
  "PatternGenerator with non-power-of-two samples" should "work" in {
    Driver(() => new PatternGenerator(8, 1, 5, false)) {
      c => new PatternGeneratorNonalignedDepthTester(c)
    } should be (true)
  }
  "PatternGenerator with multiple signals per line" should "work" in {
    Driver(() => new PatternGenerator(8, 2, 4, false)) {
      c => new PatternGeneratorMultilineTester(c)
    } should be (true)
  }
}
