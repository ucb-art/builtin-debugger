// See LICENSE for license details.

package debuggers

import chisel3._
import chisel3.util._

/** Common trigger block and utilities.
  */
class Trigger extends Module {
  val io = new Bundle {
    /** Trigger mode:
      * trigNone: triggered always high.
      * trigHigh: triggered high on first cycle where input is valid and high.
      * trigLow: triggered high on the first cycle where input is valid and low.
      * trigRising: triggered goes high on first cycle where input is valid and high following a
      * cycle where input was valid and low.
      * trigFalling: triggered goes high on first cycle where input is valid and low following a
      * cycle where input was valid and high.
      */
    val config = Input(UInt(width=3))
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
  }
}
