// See LICENSE for license details.

package debuggers

import chisel3._
import chisel3.util._

/** Trigger modes (shorthand for trigger value and valid during testing)
  */
sealed trait TriggerState
case object THigh extends TriggerState
case object TLow extends TriggerState
case object TInvalid extends TriggerState
case object TInvHigh extends TriggerState
case object TInvLow extends TriggerState

object TriggerBlock {
  sealed abstract class Mode(
    val id: Int
  ) {
    def U: UInt = id.U
  }

  object Mode {
    implicit def toInt(x: Mode) = x.id
    implicit def toBigInt(x: Mode):BigInt = x.id
    // TODO: this could be automatically generated with macros and stuff
    val all: Set[Mode] = Set(Always, High, Low, Rising, Falling)
    val width = log2Up(all.size)
  }

  /** Triggered always high, regardless of valid
    */
  case object Always extends Mode(0)
  /** Triggered high on first cycle where input is valid and high
    */
  case object High extends Mode(1)
  /** Triggered high on the first cycle where input is valid and low
    */
  case object Low extends Mode(2)
  /** Triggered goes high on first cycle where input is valid and high following a
    * cycle where input was valid and low
    */
  case object Rising extends Mode(3)
  /** Triggered goes high on first cycle where input is valid and low following a
    * cycle where input was valid and high
    */
  case object Falling extends Mode(4)

}

/** Common trigger block and utilities.
  */
class TriggerBlock extends Module {
  import TriggerBlock._

  val io = IO(new Bundle {
    /** Trigger mode, see API docs of subtypes of TriggerMode.
      */
    val config = Input(UInt(width=Mode.width))
    /** Whether the module is active or not. A low here acts as a reset for internal state, like
      * required for rising and falling triggers.
      */
    val active = Input(Bool())
    /** Input trigger signal with valid.
      */
    val input = Flipped(Valid(Bool()))
    /** Output signal. Goes high when triggered. Behavior undefined in the period between triggered
      * being high and a reset through high-low-high toggling of active.
      */
    val triggered = Output(Bool())
  })

  val lastInput = Reg(Valid(Bool()))

  when (io.active) {
    when (io.input.valid) {
      lastInput := io.input
    }
  } .otherwise {
    lastInput.valid := false.B
  }

  switch (io.config) {
  is (Always.U) {
    io.triggered := true.B
  }
  is (High.U) {
    io.triggered := io.input.valid && io.input.bits
  }
  is (Low.U) {
    io.triggered := io.input.valid && !io.input.bits
  }
  is (Rising.U) {
    io.triggered := lastInput.valid && !lastInput.bits && io.input.valid && io.input.bits
  }
  is (Falling.U) {
    io.triggered := lastInput.valid && lastInput.bits && io.input.valid && !io.input.bits
  }
  }
}
