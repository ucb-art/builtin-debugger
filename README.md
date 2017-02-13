# builtin-debugger
Make debugging great again with hardware blocks that help reduce the amount of head-meets-wall in your bring-up experience.

## Introduction
After a chip is taped out comes the process of bring-up and debugging, and if you can't see inside your chip, you're going to have a really bad time.

Re-usable infrastructure developed as part of the [CRAFT DSP chip project](https://github.com/ucb-art/craft2-chip).

### Experimental!
This is still an experimental design and the API is still subject to change.

## Overview
Two blocks are currently available, intended for rate matching between a slow external interface (like JTAG, which tops out at tens of Mbit/s) and fast internal buses (like a parallel bus running at hundreds of MHz).
- Logic analyzer: reads data off a bus and stores it in an internal memory block, to be read out at slower rates later.
- Pattern generator: puts data onto a bus based on data stored in an internal memory block.

Both are fully parameterizable (bus width, external interface width, number of samples) and have limited runtime controls (optional ready / valid interlock, optional trigger to start, programmable number of samples or continuous modes).

### Usage Guide
*To be written, once the API has stabilized. Check out the examples for now.*

### Package Structure
No guarantees are made about the contents of the `examples` folder. In particular, do NOT depend on the contents of those (such as submoduled JTAG or async tools) in your designs.

## Hardware Verification
This generator has been used in these designs:
- Example design on [ICE40HX8K-B-EVN (Lattice iCE40 FPGA) through Yosys](examples/ice40hx8k-yosys) with a [JTAG transport layer](https://github.com/ucb-art/chisel-jtag)

Planned:
- None currently.

## TODOs

- Stabilize block I/O API
