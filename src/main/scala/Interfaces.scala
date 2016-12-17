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
  class ModIO extends Bundle {
    val reset = Input(Bool())  // synchronous address reset
    val addr = Output(UInt(log2Up(count).W))  // address, of current element
    val input = Flipped(Decoupled(gen))  // data in queue
    val output = Decoupled(gen)  // data out queue

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

  io.addr := addr
  io.input.ready := io.output.ready
  io.output.valid := io.input.valid
  io.output.bits := io.input.bits
}
