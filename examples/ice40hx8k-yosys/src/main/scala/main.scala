package debuggersExamples

import chisel3._
import chisel3.util._
import chisel3.experimental.{withClockAndReset, chiselName}
import debuggers._
import jtag._

class DesignIO extends Bundle {
  val jtag = new JtagIO

  val out0 = Output(UInt(8.W))
  val out1 = Output(UInt(3.W))
  val out2 = Output(UInt(3.W))
  val out3 = Output(UInt(3.W))
}

@chiselName
class DesignTop extends Module {
  val io = IO(new DesignIO)
  val irLength = 4

  //
  // System blocks
  //
  val pg = Module(new PatternGenerator(4, 2, 32))
  val la = Module(new LogicAnalyzer(4, 2, 32))

  //
  // Timing control
  //
  val (prescaleCnt, prescaleWrap) = Counter(true.B, 1000)
  val periodCounter = Reg(UInt(16.W))
  
  val queuePeriod = Wire(Decoupled(periodCounter.chiselCloneType))
  queuePeriod.ready := true.B
  val periodMax = RegEnable(queuePeriod.bits, 500.U, queuePeriod.valid)

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
  
  val laMemReq = Wire(Decoupled(la.io.memory.reqAddr.cloneType))
  val laMemResp = Wire(Decoupled(la.io.memory.respData.cloneType))

  val laMemRespValid = RegNext(laMemReq.valid, false.B)
  laMemReq.ready := true.B
  la.io.memory.reqAddr := laMemReq.bits
  
  laMemResp.bits := la.io.memory.respData
  laMemResp.valid := laMemRespValid
  
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

    val chain1 = Module(CaptureUpdateChain(UInt(3.W)))
    val chain1reg = RegEnable(chain1.io.update.bits, 1.U, chain1.io.update.valid)
    chain1.io.capture.bits := chain1reg

    val chain2 = Module(CaptureUpdateChain(UInt(3.W)))
    val chain2reg = RegEnable(chain2.io.update.bits, 0.U, chain2.io.update.valid)
    chain2.io.capture.bits := chain2reg

    val chainPeriod = Module(new DecoupledSourceChain(queuePeriod.bits.cloneType))
    queuePeriod <> _root_.util.AsyncDecoupledTo(clock, reset, chainPeriod.io.interface, 2)

    val chainSource = Module(new DecoupledSourceChain(UInt(8.W)))
    val queue = Queue(chainSource.io.interface, 2)
    val chainSink = Module(new DecoupledSinkChain(UInt(8.W)))
    chainSink.io.interface <> queue
    
    val chainPgCtl = Module(new DecoupledSourceChain(pg.io.control.bits.cloneType))
    pg.io.control <> _root_.util.AsyncDecoupledTo(clock, reset, chainPgCtl.io.interface, 2)
    val chainPgMem = Module(new DecoupledSourceChain(pg.io.memory.bits.cloneType))
    pg.io.memory <> _root_.util.AsyncDecoupledTo(clock, reset, chainPgMem.io.interface, 2)
    
    val chainLaCtl = Module(new DecoupledSourceChain(la.io.control.bits.cloneType))
    la.io.control <> _root_.util.AsyncDecoupledTo(clock, reset, chainLaCtl.io.interface, 2)
    val chainLaMemReq = Module(new DecoupledSourceChain(laMemReq.bits.cloneType))
    laMemReq <> _root_.util.AsyncDecoupledTo(clock, reset, chainLaMemReq.io.interface, 2)
    val chainLaMemResp = Module(new DecoupledSinkChain(laMemResp.bits.cloneType))
    chainLaMemResp.io.interface <> _root_.util.AsyncDecoupledFrom(clock, reset, laMemResp, 2)
    
    val tapIo = JtagTapGenerator(irLength, Map(
          0 -> chain0,
          1 -> chain0,
          2 -> chain1,
          3 -> chain2,
          4 -> chainPeriod,
          
          5 -> chainSource,
          6 -> chainSink,

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
    
    io.out1 := chain1reg
    io.out2 := chain2reg
    
    //io.out2 := Cat(pg.io.control.valid, chainPgCtl.io.interface.valid, chainPgCtl.io.interface.ready)
    //io.out2 := Cat(chainLaMemResp.io.interface.valid, laMemResp.valid, laMemResp.ready)
    //io.out2 := Cat(chainSink.io.interface.valid, chainSource.io.interface.valid, chainSource.io.interface.ready)
  }

  //
  // Assign outputs
  //
  io.out0 := Cat(pulse, pg.io.signal.valid, la.io.status.state =/= 0.U, tapReset,
      pg.io.signal.bits)
      
  io.out3 := Cat(0.U(1.W), reset, clock.asUInt)
}

@chiselName
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
    
    io <> design.io
    
    // POR JTAG by last-connect semantics
    when (internalReset) {
      design.io.jtag.TCK := internalClock
      // Use TMS to "initialize" JTAG TAP into Run-Test-Idle state
      when (resetCnt === (resetCycles-1).asUInt) {
        design.io.jtag.TMS := false.B
      } .otherwise {
        design.io.jtag.TMS := true.B
      }
      design.io.jtag.TDI := false.B
    }
  }
}

object Top {
  def main(args: Array[String]): Unit = {
    Driver.execute(args, () => new Top)
  }
}