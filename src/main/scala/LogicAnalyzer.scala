// See LICENSE for license details.

package debuggers

import chisel3._
import chisel3.util._

/** Built-in snooping logic analyzer.
  *
  * @param dataWidth bit width of the data (signal input to sample)
  * @param memWidth bit width of memory (for read out), must be an integer multiple of dataWidth
  * @param samples number of samples that can be stored in memory, must be an integer multiple of
  * memWidth / dataWidth
  */
class LogicAnalyzer(dataWidth: Int, memWidth: Int, samples: Int) extends Module {
  assert(memWidth % dataWidth == 0)
  assert(samples % (memWidth / dataWidth) == 0)

  val samplesWidth = log2Up(samples + 1)
  val reqAddrWidth = log2Up(samples / (memWidth / dataWidth))

  // TODO: DRYify
  val sIdle :: sArmed :: sRunning :: Nil = Enum(UInt(), 3)
  val stateWidth = log2Up(3)

  // TODO: DRYify
  val trigNone, trigHigh :: trigLow :: trigRising :: trigFalling = Enum(UInt(), 5)
  val trigWidth = log2Up(5)

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
    * The memory is specified as lines of (memWidth/dataWidth) samples each, and the address in
    * memory for a particular sample is floor(sampleAddr / (memWidth/dataWidth)).
    * The lowest sample is the least significant dataWidth bits of the returned memory content.
    */
  class LogicAnalyzerMemory extends Bundle {
    // TODO: ready/valid this?
    val reqAddr = Input(UInt(width=reqAddrWidth))

    /** Memory line of the requested address. Available the cycle after the address is requested
     *  and when the logic analyzer is in the idle state.
      */
    val respData = Input(UInt(width=memWidth))

    override def cloneType: this.type = (new LogicAnalyzerMemory).asInstanceOf[this.type]
  }

   /** Logic analyzer control. Ready/Valid gated.
     */
  class LogicAnalyzerControl extends Bundle {
    /** Logic analyzer configuration: ignore the signal valid and sample every clock cycle.
      */
    val validBypass = Input(Bool())
    /** Logic analyzer configuration: trigger mode.
      * trigNone: start sampling immediately, on the next valid cycle.
      * trigHigh: start sampling on the first valid cycle where trigger is high.
      * trigLow: start sampling on the first valid cycle where trigger is low.
      * trigRising: start sampling on the first valid cycle where trigger is high, following a
      * valid cycle where trigger was low.
      * trigFalling: start sampling on the first valid cycle where trigger is low, following a
      * valid cycle where trigger was high.
      */
    val triggerMode = Input(UInt(width=trigWidth))
    /** Logic analyzer configuration: number of samples to take.
      * Zero means to run in continuous mode, wrapping around the memory write address and
      * overwriting previous samples until stopped through the control abort signal.
      */
    val numSamples = Input(UInt(width=samplesWidth))
    /** Arm the logic analyzer, latching in the logic analyzer configuration.
      * Only valid in the idle state, transitions to the armed state.
      */
    val arm = Input(Bool())
    /** Transitions back to the idle state, aborting any capture in progress.
      * Partial samples are kept and status numSampled will be valid.
      */
    val abort = Input(Bool())

    override def cloneType: this.type = (new LogicAnalyzerControl).asInstanceOf[this.type]
  }


  /** Logic analyzer status. Always valid.
    */
  class LogicAnalyzerStatus extends Bundle {
    /** Current state the logic analyzer is in.
      */
    val state = Output(UInt(width=stateWidth))
    /** Number of valid samples in the buffer, between 0 and `samples`.
      * 0 means no samples are valid.
      *
      * In continuous mode, this will roll over from `samples` to 1.
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
    val signal = new LogicAnalyzerStatus
    val memory = new LogicAnalyzerMemory
    val control = Decoupled(new LogicAnalyzerControl)
    val status = new LogicAnalyzerStatus

    override def cloneType: this.type = (new LogicAnalyzerIO).asInstanceOf[this.type]
  }

  val io = new LogicAnalyzerIO
}
