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
class PatternGenerator(dataWidth: Int, lineWidth: Int, samples: Int,
    combinationalTrigger: Boolean = true) extends Module {
  //
  // Common constants
  //
  assert(samples % lineWidth == 0)

  val memDepth = samples / lineWidth
  val reqAddrWidth = log2Up(memDepth)

  // TODO: DRYify
  val sIdle :: sArmed :: sRunning :: Nil = Enum(3)
  val stateWidth = log2Up(3)

  //
  // IO Definitions
  //
  /** Signal output with optional flow control signals.
    */
  class PatternGeneratorSignal extends Bundle {
    /** Data being played back from memory.
      */
    val data = Output(UInt(dataWidth.W))
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
    val writeAddr = UInt(reqAddrWidth.W)

    val writeData = Vec(lineWidth, UInt(dataWidth.W))

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
    val triggerMode = UInt(TriggerBlock.Mode.width.W)
    /** Pattern generator configuration: last sample to play back, or one less than the number of
      * samples to play back.
      */
    val lastSample = UInt(log2Up(samples).W)
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
    def widthInt = 4 + TriggerBlock.Mode.width + log2Up(samples)  // TODO eliminate this nasty hack
  }

  /** Pattern generator status. Always valid.
    */
  class PatternGeneratorStatus extends Bundle {
    /** Current state the pattern generator is in.
      */
    val state = Output(UInt(stateWidth.W))
    /** Index of the current sample being played (or, in idle, the last sample to be played back).
      *
      * 0 can be disambiguated as either never started or played the first sample by reading
      * `started`.
      *
      * In continuous mode, this will roll over from `lastSample` 0.
      */
    val numSampled = Output(UInt(log2Up(samples).W))
    /** True if a sample was played out (pattern generator was triggered).
      */
    val started = Output(Bool())
    /** In continuous mode, indicates if the numSampled has ever overflowed.
      *
      * Meaningless in single (non-continuous) mode.
      */
    val overflow = Output(Bool())

    override def cloneType: this.type = (new PatternGeneratorStatus).asInstanceOf[this.type]
  }

  class PatternGeneratorIO extends Bundle {
    /** Data to be sampled with (bypassable) ready signal.
      */
    val signal = Decoupled(UInt(dataWidth.W))
    /** Optional signal to trigger logic analyzer.
      */
    val trigger = Flipped(Valid(Bool()))
    val memory = Flipped(Decoupled(new PatternGeneratorMemory))
    val control = Flipped(Decoupled(new PatternGeneratorControl))
    val status = new PatternGeneratorStatus

    override def cloneType: this.type = (new PatternGeneratorIO).asInstanceOf[this.type]
  }

  val io = IO(new PatternGeneratorIO)

  //
  // Pattern Generator State
  //
  val buffer = SeqMem(memDepth, Vec(lineWidth, UInt(dataWidth.W)))
  val state = Reg(UInt(stateWidth.W), init=sIdle)
  val currSample = Reg(UInt(log2Up(samples).W))
  val overflow = Reg(Bool())
  val started = Reg(Bool())

  // Configuration bits
  val confReadyBypass = Reg(Bool())
  val confTriggerMode = Reg(UInt(TriggerBlock.Mode.width.W))
  val confLastSample = Reg(UInt(log2Up(samples).W))
  val confContinuous = Reg(Bool())

  //
  // Trigger Control
  //
  val internalReady = confReadyBypass || io.signal.ready  // advance when true

  val triggerModule = Module(new TriggerBlock)
  triggerModule.io.config := confTriggerMode
  triggerModule.io.active := state === sArmed
  triggerModule.io.input.bits := io.trigger.bits
  triggerModule.io.input.valid := io.trigger.valid

  val combinationalTriggerFire = if (combinationalTrigger) {
    state === sArmed && triggerModule.io.triggered
  } else {
    false.B
  }

  // high means a sample is currently valid
  val internalValid = state === sRunning || combinationalTriggerFire

  // high means advance sample at end of this cycle (new sample on data bus during next cycle)
  val advance = internalReady && internalValid
  val lastSample = confLastSample === currSample

  //
  // Memory Interface & Control
  //
  val nextSample = Wire(UInt(log2Up(samples).W))
  when (advance && !(io.control.bits.abort && io.control.valid)) {
    when (lastSample) {
      nextSample := 0.U
      overflow := true.B
    } .otherwise {
      nextSample := currSample + 1.U
    }
  } .otherwise {
    nextSample := currSample
  }
  currSample := nextSample

  // Memory control
  val sampleReadLine = Wire(Vec(lineWidth, UInt(dataWidth.W)))
  when (state === sIdle) {
    sampleReadLine := Vec(Seq.fill(lineWidth)(0.U))  // TODO: latch previous sample?
    io.signal.valid := false.B

    when (io.memory.valid) {
      buffer.write(io.memory.bits.writeAddr, io.memory.bits.writeData)
    }
    io.memory.ready := true.B
  } .otherwise {
    sampleReadLine := buffer.read(nextSample / lineWidth.U)
    io.signal.valid := internalValid
    io.memory.ready := false.B
  }

  //
  // Output control
  //
  io.status.state := state
  io.status.numSampled := currSample
  io.status.started := started || combinationalTriggerFire
  io.status.overflow := overflow

  io.signal.bits := sampleReadLine(currSample % lineWidth.U)

  //
  // State Machine
  //
  io.control.ready := true.B

  val nextState = Wire(UInt(stateWidth.W))

  when (io.control.bits.abort && io.control.valid) {
    nextState := sIdle
    started := started || combinationalTriggerFire
  } .elsewhen (state === sIdle && io.control.bits.arm && io.control.valid) {
    nextState := sArmed
    confReadyBypass := io.control.bits.readyBypass
    confTriggerMode := io.control.bits.triggerMode
    confLastSample := io.control.bits.lastSample
    confContinuous := io.control.bits.continuous
    currSample := 0.U
    overflow := false.B
    started := false.B
  } .elsewhen (state === sArmed && triggerModule.io.triggered) {
    nextState := sRunning
    started := true.B
  } .elsewhen (advance && lastSample && !confContinuous) {
    nextState := sIdle
  } .otherwise {
    nextState := state
  }

  state := nextState

  //
  // Interface Generators
  //
  /** A Module that presents a streaming data interface for writing into the memory. Address starts
    * at 0 and auto-increments for every memory line written, wrapping back to zero.
    *
    * Combinational translation.
    */
  class StreamingMemoryQueue extends Module {
    val outer = PatternGenerator.this

    class ModIO extends Bundle {
      val reset = Input(Bool())  // synchronous address reset
      val addr = Output(outer.io.memory.bits.writeAddr.cloneType)  // current address indicator
      val input = Flipped(Decoupled(outer.io.memory.bits.writeData.cloneType))  // data load queue

      val output = Decoupled(outer.io.memory.bits.cloneType)  // interface to PG memory
    }
    val io = IO(new ModIO)

    val addr = Reg(UInt(reqAddrWidth.W), init=0.U)
    io.addr := addr

    when (io.reset) {
      addr := 0.U
    } .elsewhen (io.input.ready && io.input.valid) {
      when (addr === memDepth.U) {
        addr := 0.U
      } .otherwise {
        addr := addr + 1.U
      }
    }

    io.input.ready := io.output.ready
    io.output.valid := io.input.valid
    io.output.bits.writeAddr := addr
    io.output.bits.writeData := io.input.bits
  }

  def createStreamingMemoryInterface() = {
    val interface = Module(new StreamingMemoryQueue)
    io.memory <> interface.io.output
    interface
  }
}
