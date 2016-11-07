// See LICENSE for license details.

package debuggers

import chisel3._
import chisel3.util._

/** Pattern generator block, replay samples written to memory onto a data bus.
  *
  * @param dataWidth bit width of the data (signal output)
  * @param lineWidth how many dataWidth bits can be set at once; width of the internal buffer
  * @param samples number of samples that can be stored in memory, must be an integer multiple of
  * lineWidth
  */
class PatternGenerator(dataWidth: Int, lineWidth: Int, samples: Int) extends Module {
  //
  // Common constants
  //
  assert(samples % lineWidth == 0)

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
  /** Signal output with optional flow control signals.
    */
  class PatternGeneratorSignal extends Bundle {
    /** Data being played back from memory.
      */
    val data = Output(UInt(width=dataWidth))
    /** High when data is valid (being played back).
      */
    val valid = Output(Bool())
    /** Optional ready signal, gating when to advance to the next sample.
      * See readyBypass in the control group.
      */
    val ready = Input(Bool())
    /** Optional trigger signal, controlling when to start playing the first signal.
      * See triggerMode in the control group.
      */
    val trigger = Input(Bool())

    override def cloneType: this.type = (new PatternGeneratorSignal).asInstanceOf[this.type]
  }

  /** Write port into memory,
    *
    * Memory may only be written while the pattern generator is in the idle state.
    *
    * The memory is specified as lines of `lineWidth` samples each, and the address in
    * memory for a particular sample is floor(sampleAddr / lineWidth).
    * The lowest sample is the least significant dataWidth bits of the returned memory content.
    */
  class PatternGeneratorMemory extends Bundle {
    val writeAddr = UInt(width=reqAddrWidth)

    val writeData = Vec(lineWidth, UInt(width=dataWidth))

    override def cloneType: this.type = (new PatternGeneratorMemory).asInstanceOf[this.type]
  }

  /** Pattern generator control group.
    */
  class PatternGeneratorControl extends Bundle {
    /** Pattern generator configuration: ignore the signal ready and advance every clock cycle.
      */
    val readyBypass = Bool()
    // TODO: DRYify with LA trigger config & block
    /** Pattern generator configuration: trigger mode.
      * trigNone: start sampling immediately, on the next valid cycle.
      * trigHigh: start sampling on the first valid cycle where trigger is high.
      * trigLow: start sampling on the first valid cycle where trigger is low.
      * trigRising: start sampling on the first valid cycle where trigger is high, following a
      * valid cycle where trigger was low.
      * trigFalling: start sampling on the first valid cycle where trigger is low, following a
      * valid cycle where trigger was high.
      */
    val triggerMode = UInt(width=trigWidth)
    /** Pattern generator configuration: last sample to play back, or one less than the number of
      * samples to play back.
      */
    val lastSample = UInt(width=log2Up(samples))
    /** Pattern generator configuration: loop back to first sample after playback hits lastSample.
      */
    val continuous = Bool()
    /** Arm the pattern generator, latching in the control configuration bits.
      * Only valid in the idle state, transitions to the armed state.
      */
    val arm = Bool()
    /** Transitions back to the idle state, aborting any playback in progress.
      */
    val abort = Bool()

    override def cloneType: this.type = (new PatternGeneratorControl).asInstanceOf[this.type]
  }

  /** Pattern generator status. Always valid.
    */
  class PatternGeneratorStatus extends Bundle {
    /** Current state the pattern generator is in.
      */
    val state = Output(UInt(width=stateWidth))
    /** Number of samples played back, between 0 and `lastSample` + 1, inclusive.
      * 0 means no samples have been played back yet.
      *
      * In continuous mode, this will roll over from `lastSample` + 1 to 1.
      */
    val numSampled = Output(UInt(log2Up(samples + 1)))
    /** In continuous mode, indicates if the numSampled has ever overflowed.
      *
      * Meaningless in single (non-continuous) mode.
      */
    val overflow = Output(Bool())

    override def cloneType: this.type = (new PatternGeneratorStatus).asInstanceOf[this.type]
  }

  class PatternGeneratorIO extends Bundle {
    val signal = new PatternGeneratorSignal
    val memory = Flipped(Decoupled(new PatternGeneratorMemory))
    val control = Flipped(Decoupled(new PatternGeneratorControl))
    val status = new PatternGeneratorStatus

    override def cloneType: this.type = (new PatternGeneratorIO).asInstanceOf[this.type]
  }

  val io = IO(new PatternGeneratorIO)

  //
  // Pattern Generator State
  //
  val buffer = SeqMem(memDepth, Vec(lineWidth, UInt(width=dataWidth)))
  val state = Reg(UInt(width=stateWidth), init=sIdle)
  val currSample = Reg(UInt(width=log2Up(samples)))
  val overflow = Reg(Bool())
  val started = Reg(Bool())

  io.status.state := state
  io.status.numSampled := Mux(started, currSample + 1.U, 0.U)
  io.status.overflow := overflow

  // Configuration bits
  val confReadyBypass = Reg(Bool())
  val confTriggerMode = Reg(UInt(width=trigWidth))
  val confLastSample = Reg(UInt(width=log2Up(samples)))
  val confContinuous = Reg(Bool())

  //
  // Trigger Control
  //
  val internalReady = confReadyBypass || io.signal.ready  // advance when true
  val lastTrigger = Reg(Bool())  // previous trigger value
  val lastTriggerValid = Reg(Bool())  // whether the previous trigger value is valid (sampled after arming)

  // TODO: DRYify with LogicAnalyzer
  when (state === sArmed) {
    lastTrigger := io.signal.trigger
    lastTriggerValid := true.B
  } .elsewhen (state === sIdle) {
    lastTriggerValid := false.B
  }

  val internalTrigger = Wire(Bool())  // high means start sampling this cycle
  switch (confTriggerMode) {
  is (trigNone) {
    internalTrigger := true.B
  }
  is (trigHigh) {
    internalTrigger := io.signal.trigger
  }
  is (trigLow) {
    internalTrigger := !io.signal.trigger
  }
  is (trigRising) {
    internalTrigger := io.signal.trigger && lastTriggerValid && !lastTrigger
  }
  is (trigFalling) {
    internalTrigger := !io.signal.trigger && lastTriggerValid && lastTrigger
  }
  }

  // high means advance sample at end of this cycle (new sample on data bus during next cycle)
  val advance = internalReady && state === sRunning
  val lastSample = confLastSample === currSample

  //
  // Memory Interface & Control
  //
  val nextSample = Wire(UInt(width=log2Up(samples)))
  when (advance) {
    when (lastSample && confContinuous) {
      // Overflow in continuous mode
      nextSample := 0.U
      overflow := true.B
    } .otherwise {
      nextSample := currSample + 1.U
    }
  } .otherwise {
    nextSample := currSample
  }

  // Memory control
  val sampleReadLine = Vec(lineWidth, UInt(width=dataWidth))
  when (state === sIdle) {
    sampleReadLine := 0.U  // TODO: latch previous sample?
    io.signal.valid := false.B

    when (io.memory.valid) {
      buffer.write(io.memory.bits.writeAddr, io.memory.bits.writeData)
    }
    io.memory.ready := true.B
  } .otherwise {
    sampleReadLine := buffer.read(nextSample / lineWidth.U)
    io.signal.valid := true.B

    io.memory.ready := false.B
  }

  //
  // Output control
  //
  io.signal.data := sampleReadLine(currSample % lineWidth.U)

  //
  // State Machine
  //
  io.control.ready := true.B

  val nextState = Wire(UInt(width=stateWidth))

  when (io.control.bits.abort && io.control.valid) {
    nextState := sIdle
  } .elsewhen (state === sIdle && io.control.bits.arm && io.control.valid) {
    nextState := sArmed
    confReadyBypass := io.control.bits.readyBypass
    confTriggerMode := io.control.bits.triggerMode
    confLastSample := io.control.bits.lastSample
    confContinuous := io.control.bits.continuous
    currSample := 0.U
    overflow := false.B
    started := false.B
  } .elsewhen (state === sArmed && internalTrigger) {
    nextState := sRunning
  } .elsewhen (advance && lastSample) {
    nextState := sIdle
  } .otherwise {
    nextState := state
  }

  state := nextState
}
