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
  val Idle, Arming, Armed, Running = Value
}
import PatternGeneratorState.PatternGeneratorStateType

// This provides a shorthand way of specifying both the expected sample and the started output
sealed trait SampleState
object NotStarted extends SampleState
case class Started(val samples: Int) extends SampleState
case class Overflow(val samples: Int) extends SampleState
object SampleState {
  implicit def convert(x: Int) = Started(x)
}

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
  }

  def generatorStep(expectedState: PatternGeneratorStateType, expectedSignal: Option[Int],
      expectedSampled: SampleState, ready: Boolean = true, trigger: TriggerState = TInvalid,
      expectedOverflow: Boolean = false) {
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
    expectedSampled match {
      case Started(sample) => {
        expect(c.io.status.numSampled, sample, "sampled mismatch")
        expect(c.io.status.started, true, "started mismatch")
        expect(c.io.status.overflow, false, "overflow mismatch")
      }
      case Overflow(sample) => {
        expect(c.io.status.numSampled, sample, "sampled mismatch")
        expect(c.io.status.started, true, "started mismatch")
        expect(c.io.status.overflow, true, "overflow mismatch")
      }
      case NotStarted => {
        expect(c.io.status.started, false, "started mismatch")
        expect(c.io.status.overflow, false, "overflow mismatch")
      }
    }

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
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Test with partial number of samples
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Test with 1 sample
  arm(true, TriggerMode.None,
      List(
        List(1)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Test with ready
  arm(false, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted, ready=false)  // trigger independent of ready
  generatorStep(PatternGeneratorState.Running, Some(1), 0, ready=false)  // stall
  generatorStep(PatternGeneratorState.Running, Some(1), 0, ready=false)  // stall
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3, ready=false)  // stall on last cycle
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Continuous mode test
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Running, Some(1), Overflow(0))
  generatorStep(PatternGeneratorState.Running, Some(2), Overflow(1))
  generatorStep(PatternGeneratorState.Running, Some(3), Overflow(2))
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(4), Overflow(3))
  generatorStep(PatternGeneratorState.Idle, None, Overflow(3))

  // Continuous mode test, non-aligned abort
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Running, Some(1), Overflow(0))
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(2), Overflow(1))
  generatorStep(PatternGeneratorState.Idle, None, Overflow(1))

  // Continuous mode test, partial depth
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(1), Overflow(0))
  generatorStep(PatternGeneratorState.Running, Some(2), Overflow(1))
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(3), Overflow(2))
  generatorStep(PatternGeneratorState.Idle, None, Overflow(2))

  // Continuous mode test, 1 sample
  arm(true, TriggerMode.None,
      List(
        List(1)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(1), Overflow(0))
  generatorStep(PatternGeneratorState.Running, Some(1), Overflow(0))
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(1), Overflow(0))
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Mid-run abort in non-continuous mode
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Idle, None, 1)
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
  generatorStep(PatternGeneratorState.Armed, None, NotStarted, trigger=TInvLow)  // disabled trigger
  generatorStep(PatternGeneratorState.Armed, None, NotStarted, trigger=TInvHigh)  // "false" trigger
  generatorStep(PatternGeneratorState.Armed, None, NotStarted, trigger=THigh)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1, trigger=TLow)  // ensure trigger doesn't impact anything
  generatorStep(PatternGeneratorState.Running, Some(3), 2, trigger=THigh)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Trigger-as-soon-as-possible test
  arm(false, TriggerMode.High,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted, trigger=THigh)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))
  
  // Pre-trigger abort
  arm(true, TriggerMode.High,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted, trigger=THigh)
  generatorStep(PatternGeneratorState.Idle, None, NotStarted)
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
  generatorStep(PatternGeneratorState.Arming, None, NotStarted)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted, trigger=TInvLow)  // disabled trigger
  generatorStep(PatternGeneratorState.Armed, None, NotStarted, trigger=TInvHigh)  // "false" trigger
  generatorStep(PatternGeneratorState.Armed, Some(1), 0, trigger=THigh)
  generatorStep(PatternGeneratorState.Running, Some(2), 1, trigger=TLow)  // ensure trigger doesn't impact anything
  generatorStep(PatternGeneratorState.Running, Some(3), 2, trigger=THigh)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Trigger-as-soon-as-possible test
  arm(false, TriggerMode.High,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Arming, None, NotStarted, trigger=THigh)  // ignored in Arming
  generatorStep(PatternGeneratorState.Armed, Some(1), 0, trigger=THigh)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))
  
  // Pre-trigger abort
  arm(true, TriggerMode.High,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Arming, None, NotStarted)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Idle, None, NotStarted)
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
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Running, Some(5), 4)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Test with partial number of samples
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Again, but non-power-of-two
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Continuous, full depth
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4),
        List(5)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Running, Some(5), 4)
  generatorStep(PatternGeneratorState.Running, Some(1), Overflow(0))
  generatorStep(PatternGeneratorState.Running, Some(2), Overflow(1))
  generatorStep(PatternGeneratorState.Running, Some(3), Overflow(2))
  generatorStep(PatternGeneratorState.Running, Some(4), Overflow(3))
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(5), Overflow(4))
  generatorStep(PatternGeneratorState.Idle, None, Overflow(4))

  // Continuous, partial depth
  arm(true, TriggerMode.None,
      List(
        List(1),
        List(2),
        List(3),
        List(4)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Running, Some(1), Overflow(0))
  generatorStep(PatternGeneratorState.Running, Some(2), Overflow(1))
  generatorStep(PatternGeneratorState.Running, Some(3), Overflow(2))
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(4), Overflow(3))
  generatorStep(PatternGeneratorState.Idle, None, Overflow(3))
}

class PatternGeneratorMultilineTester(val c: PatternGenerator) extends PeekPokeTester(c) with PatternGeneratorTestUtils {
  // Full depth
  arm(true, TriggerMode.None,
      List(
        List(1, 2),
        List(3, 4)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Partial depth
  arm(true, TriggerMode.None,
      List(
        List(1, 2),
        List(3)
      ), false)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Idle, None, Overflow(0))

  // Continuous, full depth
  arm(true, TriggerMode.None,
      List(
        List(1, 2),
        List(3, 4)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(4), 3)
  generatorStep(PatternGeneratorState.Running, Some(1), Overflow(0))
  generatorStep(PatternGeneratorState.Running, Some(2), Overflow(1))
  generatorStep(PatternGeneratorState.Running, Some(3), Overflow(2))
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(4), Overflow(3))
  generatorStep(PatternGeneratorState.Idle, None, Overflow(3))

  // Continuous, partial depth
  arm(true, TriggerMode.None,
      List(
        List(1, 2),
        List(3)
      ), true)
  generatorStep(PatternGeneratorState.Armed, None, NotStarted)
  generatorStep(PatternGeneratorState.Running, Some(1), 0)
  generatorStep(PatternGeneratorState.Running, Some(2), 1)
  generatorStep(PatternGeneratorState.Running, Some(3), 2)
  generatorStep(PatternGeneratorState.Running, Some(1), Overflow(0))
  generatorStep(PatternGeneratorState.Running, Some(2), Overflow(1))
  poke(c.io.control.bits.abort, true)
  poke(c.io.control.valid, true)
  generatorStep(PatternGeneratorState.Running, Some(3), Overflow(2))
  generatorStep(PatternGeneratorState.Idle, None, Overflow(2))
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
