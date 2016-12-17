package debuggersExamples

import chisel3._
import chisel3.util._
import debuggers._
import jtag._

class top extends Module {
  class CountIO extends Bundle {
    val count = Output(UInt(32.W))
  }

  class ModIO extends Bundle {
    val jtag = new JtagIO

    val out0 = Output(UInt(8.W))
    val out1 = Output(UInt(3.W))
    val out2 = Output(UInt(3.W))
  }

  val io = IO(new ModIO)
  val irLength = 3

  //
  // System blocks
  //
  val pg = Module(new PatternGenerator(4, 2, 32))

  //
  // TAP blocks
  //
  class JtagTapClocked (modClock: Clock, modReset: Bool)
      extends Module(override_clock=Some(modClock), override_reset=Some(modReset)) {
    val reg0 = Reg(UInt(8.W), init=0x55.U)
    val reg1 = Reg(UInt(3.W), init=5.U)
    val reg2 = Reg(UInt(3.W), init=5.U)

    val chain1 = Module(CaptureUpdateChain(UInt(8.W)))
    chain1.io.capture.bits := reg0
    when (chain1.io.update.valid) {
      reg0 := chain1.io.update.bits
    }

    val chain2 = Module(CaptureUpdateChain(UInt(3.W)))
    chain2.io.capture.bits := reg1
    when (chain2.io.update.valid) {
      reg1 := chain2.io.update.bits
    }

    val chain3 = Module(CaptureUpdateChain(UInt(3.W)))
    chain3.io.capture.bits := reg2
    when (chain3.io.update.valid) {
      reg2 := chain3.io.update.bits
    }

    val chainCtl = Module(CaptureUpdateChain(pg.io.control.bits.cloneType))

    val dataStreamer = Module(new StreamingAddressQueue(pg.io.memory.bits.writeData.cloneType, pg.memDepth))
    val chainData = Module(CaptureUpdateChain(dataStreamer.io.output.bits.addr.cloneType,
        dataStreamer.io.input.bits.cloneType))
    dataStreamer.io.input.valid := chainData.io.update.valid
    dataStreamer.io.input.bits := chainData.io.update.bits

    val tapIo = JtagTapGenerator(irLength, Map(
          chain1 -> 1,
          chain2 -> 2,
          chain3 -> 3,
          chainCtl -> 4,
          chainData -> 5
        ),
        idcode=Some((6, JtagIdcode(0xA, 0x123, 0x42))))

    class TapBlockIO(irLength: Int) extends JtagBlockIO(irLength) {
      val reg0 = Output(UInt(8.W))
      val reg1 = Output(UInt(3.W))
      val reg2 = Output(UInt(3.W))

      val queueCtl = pg.io.control.cloneType
      val queueMem = pg.io.memory.cloneType
    }

    val io = IO(new TapBlockIO(irLength))
    io.jtag <> tapIo.jtag
    io.output <> tapIo.output

    io.reg0 := reg0
    io.reg1 := reg1
    io.reg2 := reg2

    // TODO: some backpressure mechanism
    io.queueCtl.bits := chainCtl.io.update.bits
    io.queueCtl.valid := chainCtl.io.update.valid

    dataStreamer.io.reset := tapIo.output.instruction =/= 5.U
    io.queueMem.valid := dataStreamer.io.output.valid
    dataStreamer.io.output.ready := io.queueMem.ready
    io.queueMem.bits.writeAddr := dataStreamer.io.output.bits.addr
    io.queueMem.bits.writeData := dataStreamer.io.output.bits.data
  }

  // Generate arbitrary number of chained TAPs
  val tap_reset = Wire(Bool())
  val taps = List(
      Module(new JtagTapClocked(io.jtag.TCK.asClock, tap_reset))
  )
  tap_reset := taps.map(_.io.output.reset).fold(false.B)(_||_)
  for (tap <- taps) {
    tap.io.jtag.TCK := io.jtag.TCK
    tap.io.jtag.TMS := io.jtag.TMS
    tap.io.control.fsmAsyncReset := false.B
  }
  taps.head.io.jtag.TDI := io.jtag.TDI
  for (List(prev, next) <- taps sliding 2) {
    next.io.jtag.TDI := prev.io.jtag.TDO.data
  }
  io.jtag.TDO := taps.last.io.jtag.TDO

  //
  // System logic
  //
  val flip = Reg(Bool(), init=false.B)
  val count = Reg(UInt(8.W), init=0.U)
  // rocket-chip util seem to conflict with chisel3.util
  val queueCtl = _root_.util.AsyncDecoupledFrom(taps.head.clock, taps.head.reset, taps.head.io.queueCtl, 1, 2)
  val queueMem = _root_.util.AsyncDecoupledFrom(taps.head.clock, taps.head.reset, taps.head.io.queueMem, 1, 2)

  pg.io.control <> queueCtl
  pg.io.memory <> queueMem

  val (cnt, wrap) = Counter(true.B, 12000000)
  val pulse = cnt > (12000000 - 1000000).U
  val pgReady = Reg(Bool(), init=false.B)
  pg.io.trigger.valid := true.B
  pg.io.trigger.bits := pulse
  pg.io.signal.ready := pgReady
  when (wrap && pg.io.signal.valid) {
    pgReady := true.B
  } .elsewhen (pg.io.signal.valid) {
    pgReady := false.B
  }

  //
  // Assign outputs
  //
  io.out0 := Cat(pulse, pgReady, pg.io.signal.valid, 0.U(1.W), pg.io.signal.bits)
  io.out1 := taps.head.io.reg1
  io.out2 := taps.head.io.reg2
}

object Top {
  def main(args: Array[String]): Unit = {
    Driver.execute(args, () => new top)
  }
}