package debuggersExamples

import chisel3._
import chisel3.util._
import debuggers._
import jtag._

class DesignTop(modClock: Clock, modReset: Bool)
      extends Module(override_clock=Some(modClock), override_reset=Some(modReset)) {
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
  val la = Module(new LogicAnalyzer(4, 2, 32))

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

    val chainPeriod = Module(CaptureUpdateChain(Bool(), UInt(16.W)))
        
    val chainPgCtl = Module(CaptureUpdateChain(Bool(), pg.io.control.bits.cloneType))
    val chainPgMem = Module(CaptureUpdateChain(Bool(), pg.io.memory.bits.cloneType))
    
    val chainLaCtl = Module(CaptureUpdateChain(Bool(), la.io.control.bits.cloneType))
    val chainLaMem = Module(CaptureUpdateChain(la.io.memory.respData.cloneType,
        la.io.memory.reqAddr.cloneType))
    
    val tapIo = JtagTapGenerator(irLength, Map(
          chain0 -> 1,
          chain1 -> 2,
          chain2 -> 3,
          chainPeriod -> 4,
          
          chainPgCtl -> 8,
          chainPgMem -> 9,
          
          chainLaCtl -> 10,
          chainLaMem -> 11
        ),
        idcode=Some((14, JtagIdcode(0xA, 0x123, 0x42))))

    class TapBlockIO(irLength: Int) extends JtagBlockIO(irLength) {
      val reg0 = Output(UInt(8.W))
      val reg1 = Output(UInt(3.W))
      val reg2 = Output(UInt(3.W))
      val queuePeriod = Output(Decoupled(UInt(16.W)))

      val queuePgCtl = pg.io.control.cloneType
      val queuePgMem = pg.io.memory.cloneType
      
      val queueLaCtl = la.io.control.cloneType
      val queueLaMemReq = Decoupled(la.io.memory.reqAddr.cloneType)
      val queueLaMemResp = Flipped(Decoupled(la.io.memory.respData.cloneType))
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
    
    
    io.queuePgCtl.bits := chainPgCtl.io.update.bits
    io.queuePgCtl.valid := chainPgCtl.io.update.valid
    chainPgCtl.io.capture.bits := RegEnable(io.queuePgCtl.ready, false.B, chainPgCtl.io.update.valid)

    io.queuePgMem.bits := chainPgMem.io.update.bits
    io.queuePgMem.valid := chainPgMem.io.update.valid
    chainPgMem.io.capture.bits := RegEnable(io.queuePgMem.ready, false.B, chainPgMem.io.update.valid)
    
    io.queueLaCtl.bits := chainLaCtl.io.update.bits
    io.queueLaCtl.valid := chainLaCtl.io.update.valid
    chainLaCtl.io.capture.bits := RegEnable(io.queueLaCtl.ready, false.B, chainLaCtl.io.update.valid)
    
    io.queueLaMemReq.bits := chainLaMem.io.update.bits
    io.queueLaMemReq.valid := chainLaMem.io.update.valid
    chainLaMem.io.capture.bits := io.queueLaMemResp.bits
    io.queueLaMemResp.ready := chainLaMem.io.capture.capture
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
  // Timing control
  //
  val (prescaleCnt, prescaleWrap) = Counter(true.B, 1000)
  
  val queuePeriod = _root_.util.AsyncDecoupledFrom(taps.head.clock, taps.head.reset, taps.head.io.queuePeriod, 1, 2)
  queuePeriod.ready := true.B
  val periodMax = RegEnable(queuePeriod.bits, 1000.U, queuePeriod.valid)
  
  val periodCounter = Reg(UInt(16.W))
  val wrap = Wire(Bool())
  when (prescaleWrap) {
    when (periodCounter >= periodMax) {
      periodCounter := 0.U;
      wrap := true.B
    } .otherwise {
      periodCounter := periodCounter + 1.U;
      wrap := false.B
    }
  } .otherwise {
    wrap := false.B
  }
  
  val pulse = periodCounter > (periodMax - 16.U)
  
  //
  // Pattern Generator Interface
  //
  // rocket-chip util seem to conflict with chisel3.util
  val queuePgCtl = _root_.util.AsyncDecoupledFrom(taps.head.clock, taps.head.reset, taps.head.io.queuePgCtl, 1, 2)
  val queuePgMem = _root_.util.AsyncDecoupledFrom(taps.head.clock, taps.head.reset, taps.head.io.queuePgMem, 1, 2)

  pg.io.control <> queuePgCtl
  pg.io.memory <> queuePgMem

  pg.io.trigger.valid := true.B
  pg.io.trigger.bits := pulse
  pg.io.signal.ready := wrap

  //
  // Logic Analyzer Interface
  //
  val queueLaCtl = _root_.util.AsyncDecoupledFrom(taps.head.clock, taps.head.reset, taps.head.io.queueLaCtl, 1, 2)
  val queueLaMemReq = _root_.util.AsyncDecoupledFrom(taps.head.clock, taps.head.reset, taps.head.io.queueLaMemReq, 1, 2)
  val queueLaMemResp = Wire(taps.head.io.queueLaMemResp.cloneType)
  taps.head.io.queueLaMemResp <> _root_.util.AsyncDecoupledTo(taps.head.clock, taps.head.reset, queueLaMemResp, 1, 2)
  
  la.io.control <> queueLaCtl
  
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