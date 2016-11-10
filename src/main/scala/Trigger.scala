// See LICENSE for license details.

package debuggers

import chisel3._
import chisel3.util._

object TriggerBlock {
  sealed abstract class TriggerMode(
    val id: Int
  ) {
    def U: UInt = id.U
  }

  object TriggerMode {
    implicit def toInt(x: TriggerMode) = x.id
    implicit def toBigInt(x: TriggerMode):BigInt = x.id
    // TODO: this could be automatically generated with macros and stuff
    val all: Set[TriggerMode] = Set(TriggerNone, TriggerHigh, TriggerLow, TriggerRising, TriggerFalling)
    val width = log2Up(all.size)
  }

  /** Triggered always high
    */
  case object TriggerNone extends TriggerMode(0)
  /** Triggered high on first cycle where input is valid and high
    */
  case object TriggerHigh extends TriggerMode(1)
  /** Triggered high on the first cycle where input is valid and low
    */
  case object TriggerLow extends TriggerMode(2)
  /** Triggered goes high on first cycle where input is valid and high following a
    * cycle where input was valid and low
    */
  case object TriggerRising extends TriggerMode(3)
  /** Triggered goes high on first cycle where input is valid and low following a
    * cycle where input was valid and high
    */
  case object TriggerFalling extends TriggerMode(4)

}

/** Common trigger block and utilities.
  */
class TriggerBlock extends Module {
  import TriggerBlock._

  val io = IO(new Bundle {
    /** Trigger mode, see API docs of subtypes of TriggerMode.
      */
    val config = Input(UInt(width=TriggerMode.width))
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
  is (TriggerNone.U) {
    io.triggered := true.B
  }
  is (TriggerHigh.U) {
    io.triggered := io.input.valid && io.input.bits
  }
  is (TriggerLow.U) {
    io.triggered := io.input.valid && !io.input.bits
  }
  is (TriggerRising.U) {
    io.triggered := lastInput.valid && !lastInput.bits && io.input.valid && io.input.bits
  }
  is (TriggerFalling.U) {
    io.triggered := lastInput.valid && lastInput.bits && io.input.valid && !io.input.bits
  }
  }
}
