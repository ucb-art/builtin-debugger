package debuggersExamples

import chisel3._
import chisel3.util._
import chisel3.experimental.{withClockAndReset}
import debuggers._
import jtag._

class DesignIO extends Bundle {
  val jtag = new JtagIO

  val out0 = Output(UInt(8.W))
  val out1 = Output(UInt(3.W))
  val out2 = Output(UInt(3.W))
}

class DesignTop extends Module {
  val io = IO(new DesignIO)
  val irLength = 4

  //
  // System blocks
  //
  val pg = Module(new PatternGenerator(4, 2, 32))
  val la = Module(new LogicAnalyzer(4, 2, 32))

  val chain0val = Wire(UInt(8.W))
  val chain1val = Wire(UInt(3.W))
  val chain2val = Wire(UInt(3.W))
  
  //
  // Timing control
  //
  val (prescaleCnt, prescaleWrap) = Counter(true.B, 1000)

  val queuePeriod = Wire(Decoupled(prescaleCnt.cloneType))
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
  pg.io.trigger.valid := true.B
  pg.io.trigger.bits := wrap
  pg.io.signal.ready := wrap
  
  //
  // Logic Analyzer Interface
  //
  la.io.trigger.valid := true.B
  la.io.trigger.bits := wrap
  la.io.signal.bits := pg.io.signal.bits
  la.io.signal.valid := wrap
  
  // Sequence the request and response queues such that the request queue isn't drained until
  // the response queue is drained.
  val laMemReq = Wire(Decoupled(la.io.memory.reqAddr.cloneType))
  val laMemResp = Wire(Decoupled(la.io.memory.respData.cloneType))
  
  val respSeq = RegInit(UInt(2.W), 0.U)
  when (respSeq === 1.U) {  // wait to enqueue response
    laMemReq.ready := false.B
    laMemResp.valid := true.B
    when (laMemResp.ready) {
      respSeq := 2.U
    }
  } .elsewhen (respSeq === 2.U) {  // wait to dequeue request
    laMemReq.ready := laMemResp.ready
    laMemResp.valid := false.B
    when (laMemResp.ready) {
      respSeq := 0.U
    }
  } .otherwise {  // idle
    laMemReq.ready := false.B
    laMemResp.valid := false.B
    when (laMemReq.valid) {
      respSeq := 1.U
    }
  }
  laMemResp.bits := la.io.memory.respData
  la.io.memory.reqAddr := laMemReq.bits
  
  //
  // TAP blocks
  //
  // Generate TAP
  val tapReset = Wire(Bool())
  val tapClock = io.jtag.TCK.asClock
  withClockAndReset(tapClock, tapReset) {
    val chain0 = Module(CaptureUpdateChain(UInt(8.W)))
    val chain0reg = RegEnable(chain0.io.update.bits, 0.U, chain0.io.update.valid)
    chain0.io.capture.bits := chain0reg
    chain0val := chain0reg

    val chain1 = Module(CaptureUpdateChain(UInt(3.W)))
    val chain1reg = RegEnable(chain1.io.update.bits, 1.U, chain1.io.update.valid)
    chain1.io.capture.bits := chain1reg
    chain1val := chain1reg

    val chain2 = Module(CaptureUpdateChain(UInt(3.W)))
    val chain2reg = RegEnable(chain2.io.update.bits, 0.U, chain2.io.update.valid)
    chain2.io.capture.bits := chain2reg
    chain2val := chain2reg

    val chainPeriod = Module(new DecoupledSourceChain(queuePeriod.bits.cloneType))
    queuePeriod <> _root_.util.AsyncDecoupledTo(clock, reset, chainPeriod.io.interface, 1)

    val chainPgCtl = Module(new DecoupledSourceChain(pg.io.control.bits.cloneType))
    pg.io.control <> _root_.util.AsyncDecoupledTo(clock, reset, chainPgCtl.io.interface, 1)
    val chainPgMem = Module(new DecoupledSourceChain(pg.io.memory.bits.cloneType))
    pg.io.memory <> _root_.util.AsyncDecoupledTo(clock, reset, chainPgMem.io.interface, 1)
    
    val chainLaCtl = Module(new DecoupledSourceChain(la.io.control.bits.cloneType))
    la.io.control <> _root_.util.AsyncDecoupledTo(clock, reset, chainLaCtl.io.interface, 1)
    val chainLaMemReq = Module(new DecoupledSourceChain(laMemReq.bits.cloneType))
    laMemReq <> _root_.util.AsyncDecoupledTo(clock, reset, chainLaMemReq.io.interface, 1)
    val chainLaMemResp = Module(new DecoupledSinkChain(laMemResp.bits.cloneType))
    chainLaMemResp.io.interface <> _root_.util.AsyncDecoupledFrom(clock, reset, laMemResp, 1)
    
    val tapIo = JtagTapGenerator(irLength, Map(
          0 -> chain0,
          1 -> chain0,
          2 -> chain1,
          3 -> chain2,
          4 -> chainPeriod,

          8 -> chainPgCtl,
          9 -> chainPgMem,

          10 -> chainLaCtl,
          11 -> chainLaMemReq,
          12 -> chainLaMemResp
        ),
        idcode=Some((14, JtagIdcode(0xA, 0x123, 0x42))))
        
    tapIo.jtag <> io.jtag
    tapReset := tapIo.output.reset
    tapIo.control.fsmAsyncReset := false.B
  }

  //
  // Assign outputs
  //
  io.out0 := Cat(pulse, pg.io.signal.valid, la.io.status.state =/= 0.U, 0.U(1.W),
      pg.io.signal.bits)
  io.out1 := chain1val
  io.out2 := chain0val
}

class Top extends Module {
  val io = IO(new DesignIO)
  
  //
  // POR Reset Generator
  //
  // Clock and reset lines are registered so we have nice, smooth edges.
  val clockPrescale = 12
  require(clockPrescale % 2 == 0)
  val resetCycles = 20

  val (_, clockWrap) = Counter(true.B, clockPrescale / 2)
  val internalClock = RegInit(false.B)
  when (clockWrap) {
    internalClock := !internalClock
  }
  val resetCnt = RegInit(UInt(log2Ceil(resetCycles*2+1).W), 0.U)
  val internalReset = Reg(Bool())
  when (resetCnt < resetCycles.asUInt) {
    internalReset := true.B
    when (clockWrap) {
      resetCnt := resetCnt + 1.U
    }
  } .otherwise {
    internalReset := false.B
  }

  withClockAndReset (internalClock.asClock, internalReset) {
    val design = Module(new DesignTop)
    
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

    io <> design.io
  }
}

object Top {
  def main(args: Array[String]): Unit = {
    Driver.execute(args, () => new Top)
  }
}