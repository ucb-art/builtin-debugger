// See LICENSE for license details.

package debuggers

import chisel3._
import chisel3.util._

/** A Module that presents a streaming data interface for data that also expects an address.
  * The output address starts at zero, and increments every time both ready and valid are high.
  *
  * Combinational translation.
  */
class StreamingAddressQueue[T <: Data](gen: T, count: Int) extends Module {
  class WithAddress(gen: T) extends Bundle {
    val addr = UInt(log2Up(count).W)
    val data = gen

    override def cloneType = (new WithAddress(gen)).asInstanceOf[this.type]
  }

  class ModIO extends Bundle {
    val reset = Input(Bool())  // synchronous address reset
    val input = Flipped(Decoupled(gen))  // data in queue
    val output = Decoupled(new WithAddress(gen))  // data out queue

    override def cloneType = (new ModIO).asInstanceOf[this.type]
  }
  val io = IO(new ModIO)

  val addr = Reg(UInt(log2Up(count).W), init=0.U)

  when (io.reset) {
    addr := 0.U
  } .elsewhen (io.input.ready && io.input.valid) {
    when (addr === (count - 1).U) {
      addr := 0.U
    } .otherwise {
      addr := addr + 1.U
    }
  }

  io.input.ready := io.output.ready
  io.output.valid := io.input.valid
  io.output.bits.addr := addr
  io.output.bits.data := io.input.bits
}
