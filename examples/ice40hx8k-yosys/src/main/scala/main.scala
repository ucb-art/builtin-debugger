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
    class LaMemRespCapture extends Bundle {
      val updateReady = Bool()
      val captureValid = Bool()
      val bits = la.io.memory.respData.cloneType
    }
    val chainLaMem = Module(CaptureUpdateChain(new LaMemRespCapture(),
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
    chainLaMem.io.capture.bits.updateReady := RegEnable(io.queueLaMemReq.ready, false.B, chainLaMem.io.update.valid)
    chainLaMem.io.capture.bits.captureValid := io.queueLaMemResp.valid
    chainLaMem.io.capture.bits.bits := io.queueLaMemResp.bits
    io.queueLaMemResp.ready := chainLaMem.io.capture.capture
  }

  // Generate TAP
  val tap_reset = Wire(Bool())
  val tap = Module(new JtagTapClocked(io.jtag.TCK.asClock, tap_reset))
  tap_reset := tap.io.output.reset
  tap.io.jtag.TCK := io.jtag.TCK
  tap.io.jtag.TMS := io.jtag.TMS
  tap.io.control.fsmAsyncReset := false.B
  tap.io.jtag.TDI := io.jtag.TDI
  io.jtag.TDO := tap.io.jtag.TDO

  //
  // Timing control
  //
  val (prescaleCnt, prescaleWrap) = Counter(true.B, 1000)

  val queuePeriod = _root_.util.AsyncDecoupledFrom(tap.clock, tap.reset, tap.io.queuePeriod, 1)
  queuePeriod.ready := true.B
  val periodMax = RegEnable(queuePeriod.bits, 500.U, queuePeriod.valid)

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
  val queuePgCtl = _root_.util.AsyncDecoupledFrom(tap.clock, tap.reset, tap.io.queuePgCtl, 1)
  val queuePgMem = _root_.util.AsyncDecoupledFrom(tap.clock, tap.reset, tap.io.queuePgMem, 1)

  pg.io.control <> queuePgCtl
  pg.io.memory <> queuePgMem

  pg.io.trigger.valid := true.B
  pg.io.trigger.bits := wrap
  pg.io.signal.ready := wrap

  //
  // Logic Analyzer Interface
  //
  val queueLaCtl = _root_.util.AsyncDecoupledFrom(tap.clock, tap.reset, tap.io.queueLaCtl, 1)
  val queueLaMemReq = _root_.util.AsyncDecoupledFrom(tap.clock, tap.reset, tap.io.queueLaMemReq, 1)
  val queueLaMemResp = Wire(tap.io.queueLaMemResp.cloneType)

  la.io.control <> queueLaCtl
  la.io.memory.reqAddr := queueLaMemReq.bits

  // Sequence the request and response queues such that the request queue isn't drained until
  // the response queue is drained.
  val respSeq = Reg(UInt(2.W), init=0.U)
  when (respSeq === 1.U) {  // wait to enqueue response
    queueLaMemReq.ready := false.B
    queueLaMemResp.valid := true.B
    when (queueLaMemResp.ready) {
      respSeq := 2.U
    }
  } .elsewhen (respSeq === 2.U) {  // wait to dequeue request
    queueLaMemReq.ready := queueLaMemResp.ready
    queueLaMemResp.valid := false.B
    when (queueLaMemResp.ready) {
      respSeq := 0.U
    }
  } .otherwise {  // idle
    queueLaMemReq.ready := false.B
    queueLaMemResp.valid := false.B
    when (queueLaMemReq.valid) {
      respSeq := 1.U
    }
  }
  queueLaMemResp.bits := la.io.memory.respData

  val tapQueueLaMemResp = _root_.util.AsyncDecoupledTo(tap.clock, tap.reset, queueLaMemResp, 1)
  tap.io.queueLaMemResp <> tapQueueLaMemResp

  la.io.trigger.valid := true.B
  la.io.trigger.bits := wrap
  la.io.signal.bits := pg.io.signal.bits
  la.io.signal.valid := wrap

  //
  // Assign outputs
  //
  io.out0 := Cat(pulse, pg.io.signal.valid, la.io.status.state =/= 0.U, tapQueueLaMemResp.valid, // 0.U(1.W),
      pg.io.signal.bits)
  io.out1 := tap.io.reg1
  io.out2 := tap.io.reg2
}

class Top extends Module {
  //
  // POR Reset Generator
  //
  // Clock and reset lines are registered so we have nice, smooth edges.
  val clockPrescale = 12
  require(clockPrescale % 2 == 0)
  val resetCycles = 20

  val (_, clockWrap) = Counter(true.B, clockPrescale / 2)
  val internalClock = Reg(Bool(), init=false.B)
  when (clockWrap) {
    internalClock := !internalClock
  }
  val resetCnt = Reg(UInt(log2Up(resetCycles*2+1).W), init=0.U)
  val internalReset = Reg(Bool())
  when (resetCnt < resetCycles.asUInt) {
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
    when (resetCnt < (resetCycles-1).asUInt) {
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