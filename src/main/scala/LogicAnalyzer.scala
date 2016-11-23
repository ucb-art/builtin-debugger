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
  * @param combinationalTrigger is true, start sampling on the trigger clock cycle (and may result
  * in a longer combinational path), otherwise sampling on the next clock cycle.
  */
class LogicAnalyzer(dataWidth: Int, lineWidth: Int, samples: Int,
    combinationalTrigger: Boolean = true) extends Module {
  //
  // Common constants
  //
  assert(samples % lineWidth == 0)

  val samplesWidth = log2Up(samples + 1)
  val memDepth = samples / lineWidth
  val reqAddrWidth = log2Up(memDepth)

  // TODO: DRYify
  val sIdle :: sArmed :: sRunning :: Nil = Enum(UInt(), 3)
  val stateWidth = log2Up(3)

  //
  // IO Definitions
  //
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
    val reqAddr = Input(UInt(reqAddrWidth.W))

    /** Memory line of the requested address. Available the cycle after the address is requested
      * and when the logic analyzer is in the idle state.
      */
    val respData = Output(Vec(lineWidth, UInt(dataWidth.W)))

    override def cloneType: this.type = (new LogicAnalyzerMemory).asInstanceOf[this.type]
  }

   /** Logic analyzer control group.
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
    val triggerMode = UInt(TriggerBlock.Mode.width.W)
    /** Logic analyzer configuration: number of samples to take.
      * Zero means to run in continuous mode, wrapping around the memory write address and
      * overwriting previous samples until stopped through the control abort signal.
      */
    val numSamples = UInt(samplesWidth.W)
    /** Arm the logic analyzer, latching in the control configuration bits.
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
    val state = Output(UInt(stateWidth.W))
    /** Number of valid samples in the buffer, between 0 and `samples`, inclusive.
      * 0 means no samples are valid.
      *
      * In continuous mode, this will roll over from `samples` to 1, indicating the next memory
      * address to be written.
      */
    val numSampled = Output(UInt(samplesWidth.W))
    /** In continuous mode, indicates if the numSampled has ever overflowed.
      * Alternatively put, indicates if all contents of memory are from the latest run.
      *
      * Meaningless in single (non-continuous) mode.
      */
    val overflow = Output(Bool())

    override def cloneType: this.type = (new LogicAnalyzerStatus).asInstanceOf[this.type]
  }

  class LogicAnalyzerIO extends Bundle {
    /** Data to be sampled with (bypassable) valid signal.
      */
    val signal = Flipped(Valid(UInt(dataWidth.W)))
    /** Optional signal to trigger logic analyzer.
      */
    val trigger = Flipped(Valid(Bool()))
    val memory = new LogicAnalyzerMemory
    val control = Flipped(Decoupled(new LogicAnalyzerControl))
    val status = new LogicAnalyzerStatus

    override def cloneType: this.type = (new LogicAnalyzerIO).asInstanceOf[this.type]
  }

  val io = IO(new LogicAnalyzerIO)

  //
  // Logic Analyzer State
  //
  val buffer = SeqMem(memDepth, Vec(lineWidth, UInt(dataWidth.W)))
  val state = Reg(UInt(stateWidth.W), init=sIdle)
  val nextState = Wire(UInt(stateWidth.W))
  val nextSample = Reg(UInt(samplesWidth.W))
  val overflow = Reg(Bool())

  io.status.state := state
  io.status.numSampled := nextSample
  io.status.overflow := overflow

  // Configuration bits
  val confValidBypass = Reg(Bool())
  val confTriggerMode = Reg(UInt(TriggerBlock.Mode.width.W))
  val confNumSamples = Reg(UInt(samplesWidth.W))

  //
  // Trigger Control
  //
  val internalValid = confValidBypass || io.signal.valid  // sample when true

  val triggerModule = Module(new TriggerBlock)
  triggerModule.io.config := confTriggerMode
  triggerModule.io.active := state === sArmed
  triggerModule.io.input.bits := io.trigger.bits
  triggerModule.io.input.valid := io.trigger.valid

  // high means sample this cycle
  val sample = if (combinationalTrigger) {
    internalValid && (state === sRunning || (state === sArmed && triggerModule.io.triggered))
  } else {
    internalValid && state === sRunning
  }

  //
  // Memory Interface & Control
  //
  when (sample) {
    when (confNumSamples === 0.U && nextSample === samples.U) {
      // Overflow in continuous mode
      nextSample := 1.U
      overflow := true.B
    } .otherwise {
      nextSample := nextSample + 1.U
    }
  }

  // Line write control
  val memWriteData = Wire(Vec(lineWidth, UInt(dataWidth.W)))
  val memWriteControl = Wire(Vec(lineWidth, Bool()))
  for (i <- 0 until lineWidth) {
    when (nextSample % lineWidth.U === i.U) {
      memWriteData(i) := io.signal.bits
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
      val actualNextSample = Mux(nextSample === samples.U, 0.U, nextSample)
      // Account for line width
      val memNextAddress = actualNextSample / lineWidth.U
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
    nextSample := 0.U
    overflow := false.B
  } .elsewhen (sample && (nextSample + 1.U) === confNumSamples && confNumSamples =/= 0.U) {
    // This takes priority over Armed -> Running in the one-sample case
    // TODO: double check this logic
    nextState := sIdle
  } .elsewhen (state === sArmed && triggerModule.io.triggered) {
    nextState := sRunning
  } .otherwise {
    nextState := state
  }

  state := nextState
}
