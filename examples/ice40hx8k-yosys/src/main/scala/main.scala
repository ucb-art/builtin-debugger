package debuggersExamples

import chisel3._
import chisel3.util._
import debuggers._
import jtag._

class Top extends Module {
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
  val irLength = 4

  //
  // System blocks
  //
  val pg = Module(new PatternGenerator(4, 2, 32))

  //
  // TAP blocks
  //
  class JtagTapClocked (modClock: Clock, modReset: Bool)
      extends Module(override_clock=Some(modClock), override_reset=Some(modReset)) {
    val chain0 = Module(CaptureUpdateChain(UInt(8.W)))
    val reg0 = RegEnable(chain0.io.update.bits, 0.U, chain0.io.update.valid)
    chain0.io.capture.bits := reg0

    val chain1 = Module(CaptureUpdateChain(UInt(3.W)))
    val reg1 = RegEnable(chain1.io.update.bits, 1.U, chain1.io.update.valid)
    chain1.io.capture.bits := reg1
    
    val chain2 = Module(CaptureUpdateChain(UInt(3.W)))
    val reg2 = RegEnable(chain2.io.update.bits, 0.U, chain2.io.update.valid)
    chain2.io.capture.bits := reg2

    val chainPeriod = Module(CaptureUpdateChain(Bool(), UInt(32.W)))
        
    val chainCtl = Module(CaptureUpdateChain(Bool(), pg.io.control.bits.cloneType))
    val chainMem = Module(CaptureUpdateChain(Bool(), pg.io.memory.bits.cloneType))

    val tapIo = JtagTapGenerator(irLength, Map(
          chain0 -> 1,
          chain1 -> 2,
          chain2 -> 3,
          chainPeriod -> 4,
          
          chainCtl -> 8,
          chainMem -> 9
        ),
        idcode=Some((14, JtagIdcode(0xA, 0x123, 0x42))))

    class TapBlockIO(irLength: Int) extends JtagBlockIO(irLength) {
      val reg0 = Output(UInt(8.W))
      val reg1 = Output(UInt(3.W))
      val reg2 = Output(UInt(3.W))
      val queuePeriod = Output(Decoupled(UInt(32.W)))

      val queueCtl = pg.io.control.cloneType
      val queueMem = pg.io.memory.cloneType
    }

    val io = IO(new TapBlockIO(irLength))
    io.jtag <> tapIo.jtag
    io.output <> tapIo.output

    io.reg0 := reg0
    io.reg1 := reg1
    io.reg2 := reg2

    io.queuePeriod.bits := chainPeriod.io.update.bits
    io.queuePeriod.valid := chainPeriod.io.update.valid
    chainPeriod.io.capture.bits := RegEnable(io.queuePeriod.ready, false.B, chainPeriod.io.update.valid)
    
    
    io.queueCtl.bits := chainCtl.io.update.bits
    io.queueCtl.valid := chainCtl.io.update.valid
    chainCtl.io.capture.bits := RegEnable(io.queueCtl.ready, false.B, chainCtl.io.update.valid)

    io.queueMem.bits := chainMem.io.update.bits
    io.queueMem.valid := chainMem.io.update.valid
    chainMem.io.capture.bits := RegEnable(io.queueMem.ready, false.B, chainMem.io.update.valid)
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

  //
  // Timing control
  //
  val (_, usWrap) = Counter(true.B, 12)

  val queuePeriod = _root_.util.AsyncDecoupledFrom(taps.head.clock, taps.head.reset, taps.head.io.queuePeriod, 1, 2)
  queuePeriod.ready := true.B
  val usMax = RegEnable(queuePeriod.bits, 1000000.U, queuePeriod.valid)
  
  val usCounter = Reg(UInt(32.W))
  val wrap = Wire(Bool())
  when (usWrap) {
    when (usCounter >= usMax) {
      usCounter := 0.U;
      wrap := true.B
    } .otherwise {
      usCounter := usCounter + 1.U;
      wrap := false.B
    }
  } .otherwise {
    wrap := false.B
  }
  
  val pulse = usCounter > (usMax - 10000.U)
  val pgReady = Reg(Bool(), init=false.B)
  pg.io.trigger.valid := true.B
  pg.io.trigger.bits := pulse
  pg.io.signal.ready := wrap

  //
  // Assign outputs
  //
  io.out0 := Cat(pulse, pgReady, pg.io.signal.valid, 0.U(1.W), pg.io.signal.bits)
  io.out1 := taps.head.io.reg1
  io.out2 := taps.head.io.reg2
}

object Top {
  def main(args: Array[String]): Unit = {
    Driver.execute(args, () => new Top)
  }
}