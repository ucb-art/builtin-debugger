package debuggersExamples

import chisel3._
import chisel3.util._
import debuggers._
import jtag._

class DesignTop(modClock: Clock, modReset: Bool)
      extends Module(override_clock=Some(modClock), override_reset=Some(modReset)) {
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
  class JtagTapClocked(modClock: Clock, modReset: Bool)
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
  val queuePeriod = _root_.util.AsyncDecoupledFrom(taps.head.clock, taps.head.reset, taps.head.io.queuePeriod, 1, 2)
  queuePeriod.ready := true.B
  val periodMax = RegEnable(queuePeriod.bits, 1000000.U, queuePeriod.valid)
  
  val periodCounter = Reg(UInt(24.W))
  val wrap = Wire(Bool())
  when (periodCounter >= periodMax) {
    periodCounter := 0.U;
    wrap := true.B
  } .otherwise {
    periodCounter := periodCounter + 1.U;
    wrap := false.B
  }
  
  val pulse = periodCounter > (periodMax - 8192.U)
  pg.io.trigger.valid := true.B
  pg.io.trigger.bits := pulse
  pg.io.signal.ready := wrap

  //
  // Assign outputs
  //
  io.out0 := Cat(pulse, pg.io.signal.valid, 0.U(2.W), pg.io.signal.bits)
  io.out1 := taps.head.io.reg1
  io.out2 := taps.head.io.reg2
}

class Top extends Module {
  //
  // POR Reset Generator
  //
  // Clock and reset lines are registered so we have nice, smooth edges.
  val (clockCnt, clockWrap) = Counter(true.B, 12)
  val internalClock = Reg(Bool())
  internalClock := clockCnt < 6.U
  val resetCnt = Reg(UInt(4.W), init=0.U)
  val internalReset = Reg(Bool())
  when (resetCnt < 10.U) {
    internalReset := true.B
    when (clockWrap) {
      resetCnt := resetCnt + 1.U
    }
  } .otherwise {
    internalReset := false.B
  }
 
  val design = Module(new DesignTop(internalClock.asClock, internalReset))
  val io = IO(design.io.cloneType)
  io <> design.io
  
  when (internalReset) {
    design.io.jtag.TCK := internalClock
    // Use TMS to "initialize" JTAG TAP into Run-Test-Idle state
    when (resetCnt < 9.U) {
      design.io.jtag.TMS := true.B
    } .otherwise {
      design.io.jtag.TMS := false.B
    }
    design.io.jtag.TDI := false.B
  } .otherwise {
    design.io.jtag <> io.jtag
  }
}

object Top {
  def main(args: Array[String]): Unit = {
    Driver.execute(args, () => new Top)
  }
}