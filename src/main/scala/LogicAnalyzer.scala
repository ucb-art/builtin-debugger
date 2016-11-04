// See LICENSE for license details.

package debuggers

import chisel3._
import chisel3.util._

/** Snooping logic analyzer block.
  *
  * @param dataWidth bit width of the data (signal input to sample)
  * @param lineWidth how many dataWidth bits can be read out at once; width of the internal buffer
  * @param samples number of samples that can be stored in memory, must be an integer multiple of
  * lineWidth
  */
class LogicAnalyzer(dataWidth: Int, lineWidth: Int, samples: Int) extends Module {
  assert(samples % lineWidth == 0)

  //
  // Common constants
  //

  val samplesWidth = log2Up(samples + 1)
  val memDepth = samples / lineWidth
  val reqAddrWidth = log2Up(memDepth)

  // TODO: DRYify
  val sIdle :: sArmed :: sRunning :: Nil = Enum(UInt(), 3)
  val stateWidth = log2Up(3)

  // TODO: DRYify
  val trigNone :: trigHigh :: trigLow :: trigRising :: trigFalling :: Nil = Enum(UInt(), 5)
  val trigWidth = log2Up(5)

  //
  // IO Definitions
  //

  /** Signal input being sampled with optional flow control signals.
    */
  class LogicAnalyzerSignal extends Bundle {
    /** Data that is sampled and stored to memory.
      */
    val data = Input(UInt(width=dataWidth))
    /** Optional valid signal, gating when data is sampled to memory.
     *  See validBypass in control group.
      */
    val valid = Input(Bool())
    /** Optional trigger signal, controlling when to start sampling.
     *  See triggerBypass and triggerMode in control group.
      */
    val trigger = Input(Bool())

    override def cloneType: this.type = (new LogicAnalyzerSignal).asInstanceOf[this.type]
  }

  /** Read port into memory.
    *
    * Memory may only be read while the logic analyzer is in the idle state.
    *
    * The memory is specified as lines of `lineWidth` samples each, and the address in
    * memory for a particular sample is floor(sampleAddr / lineWidth).
    * The lowest sample is the least significant dataWidth bits of the returned memory content.
    */
  class LogicAnalyzerMemory extends Bundle {
    // TODO: ready/valid this?
    val reqAddr = Input(UInt(width=reqAddrWidth))

    /** Memory line of the requested address. Available the cycle after the address is requested
     *  and when the logic analyzer is in the idle state.
      */
    val respData = Output(Vec(lineWidth, UInt(width=dataWidth)))

    override def cloneType: this.type = (new LogicAnalyzerMemory).asInstanceOf[this.type]
  }

   /** Logic analyzer control. Ready/Valid gated.
     */
  class LogicAnalyzerControl extends Bundle {
    /** Logic analyzer configuration: ignore the signal valid and sample every clock cycle.
      */
    val validBypass = Bool()
    /** Logic analyzer configuration: trigger mode.
      * trigNone: start sampling immediately, on the next valid cycle.
      * trigHigh: start sampling on the first valid cycle where trigger is high.
      * trigLow: start sampling on the first valid cycle where trigger is low.
      * trigRising: start sampling on the first valid cycle where trigger is high, following a
      * valid cycle where trigger was low.
      * trigFalling: start sampling on the first valid cycle where trigger is low, following a
      * valid cycle where trigger was high.
      */
    val triggerMode = UInt(width=trigWidth)
    /** Logic analyzer configuration: number of samples to take.
      * Zero means to run in continuous mode, wrapping around the memory write address and
      * overwriting previous samples until stopped through the control abort signal.
      */
    val numSamples = UInt(width=samplesWidth)
    /** Arm the logic analyzer, latching in the logic analyzer configuration.
      * Only valid in the idle state, transitions to the armed state.
      */
    val arm = Bool()
    /** Transitions back to the idle state, aborting any capture in progress.
      * Partial samples are kept and status numSampled will be valid.
      */
    val abort = Bool()

    override def cloneType: this.type = (new LogicAnalyzerControl).asInstanceOf[this.type]
  }


  /** Logic analyzer status. Always valid.
    */
  class LogicAnalyzerStatus extends Bundle {
    /** Current state the logic analyzer is in.
      */
    val state = Output(UInt(width=stateWidth))
    /** Number of valid samples in the buffer, between 0 and `samples`, inclusive.
      * 0 means no samples are valid.
      *
      * In continuous mode, this will roll over from `samples` to 1, indicating the next memory
      * address to be written.
      */
    val numSampled = Output(UInt(log2Up(samples + 1)))
    /** In continuous mode, indicates if the numSampled has ever overflowed.
      * Alternatively put, indicates if all contents of memory are from the latest run.
      *
      * Meaningless in single (non-continuous) mode.
      */
    val overflow = Output(Bool())

    override def cloneType: this.type = (new LogicAnalyzerStatus).asInstanceOf[this.type]
  }

  class LogicAnalyzerIO extends Bundle {
    val signal = new LogicAnalyzerSignal
    val memory = new LogicAnalyzerMemory
    val control = Flipped(Decoupled(new LogicAnalyzerControl))
    val status = new LogicAnalyzerStatus

    override def cloneType: this.type = (new LogicAnalyzerIO).asInstanceOf[this.type]
  }

  val io = IO(new LogicAnalyzerIO)

  //
  // Logic Analyzer Logic
  //

  val buffer = SeqMem(memDepth, Vec(lineWidth, UInt(width=dataWidth)))
  val state = Reg(UInt(width=stateWidth), init=sIdle)
  val nextState = Wire(UInt(width=stateWidth))
  val nextAddress = Reg(UInt(width=samplesWidth))
  val overflow = Reg(Bool())

  io.status.state := state
  io.status.numSampled := nextAddress
  io.status.overflow := overflow

  // Logic Analyzer Configuration
  val confValidBypass = Reg(Bool())
  val confTriggerMode = Reg(UInt(width=trigWidth))
  val confNumSamples = Reg(UInt(width=samplesWidth))

  //
  // Sampling Control
  //
  val internalValid = confValidBypass || io.signal.valid  // sample when true
  val lastTrigger = Reg(Bool())  // previous trigger value
  val lastTriggerValid = Reg(Bool())  // whether the previous trigger value is valid (sampled after arming)

  when (internalValid && state === sArmed) {
    lastTrigger := io.signal.trigger
    lastTriggerValid := true.B
  } .elsewhen (state === sIdle) {
    lastTriggerValid := false.B
  }

  val internalTrigger = Wire(Bool())  // high means start sampling this cycle
  switch (confTriggerMode) {
  is (trigNone) {
    internalTrigger := internalValid
  }
  is (trigHigh) {
    internalTrigger := internalValid && io.signal.trigger
  }
  is (trigLow) {
    internalTrigger := internalValid && !io.signal.trigger
  }
  is (trigRising) {
    internalTrigger := internalValid && io.signal.trigger && lastTriggerValid && !lastTrigger
  }
  is (trigFalling) {
    internalTrigger := internalValid && !io.signal.trigger && lastTriggerValid && lastTrigger
  }
  }

  // high means sample this cycle
  val sample = internalValid && (state === sRunning || (state === sArmed && internalTrigger))

  //
  // Memory Interface & Control
  //
  when (sample) {
    when (confNumSamples === 0.U && nextAddress === samples.U) {
      // Overflow in continuous mode
      nextAddress := 1.U
      overflow := true.B
    } .otherwise {
      nextAddress := nextAddress + 1.U
    }
  }

  // Line write control
  val memWriteData = Wire(Vec(lineWidth, UInt(width=dataWidth)))
  val memWriteControl = Wire(Vec(lineWidth, Bool()))
  for (i <- 0 until lineWidth) {
    when (nextAddress % lineWidth.U === i.U) {
      memWriteData(i) := io.signal.data
      memWriteControl(i) := true.B
    } .otherwise {
      memWriteData(i) := 0.U
      memWriteControl(i) := false.B
    }
  }

  // Memory control
  when (state === sIdle) {
   io.memory.respData := buffer.read(io.memory.reqAddr)
  } .otherwise {
    when (sample) {
      // In continuous mode, last sample is first sample
      val actualNextAddress = Mux(nextAddress === samples.U, 0.U, nextAddress)
      // Account for line width
      val memNextAddress = actualNextAddress / lineWidth.U
      buffer.write(memNextAddress, memWriteData, memWriteControl)
    }
  }

  //
  // State Machine
  //
  io.control.ready := true.B
  when (io.control.bits.abort && io.control.valid) {
    nextState := sIdle
  } .elsewhen (state === sIdle && io.control.bits.arm && io.control.valid) {
    nextState := sArmed
    confValidBypass := io.control.bits.validBypass
    confTriggerMode := io.control.bits.triggerMode
    confNumSamples := io.control.bits.numSamples
    nextAddress := 0.U
    overflow := false.B
  } .elsewhen (sample && (nextAddress + 1.U) === confNumSamples && confNumSamples =/= 0.U) {
    // This takes priority over Armed -> Running in the one-sample case
    // TODO: double check this logic
    nextState := sIdle
  } .elsewhen (state === sArmed && internalTrigger) {
    nextState := sRunning
  } .otherwise {
    nextState := state
  }

  state := nextState
}
