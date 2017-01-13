# builtin-debugger
Make debugging great again with hardware blocks that help reduce the amount of head-meets-wall in your bring-up experience.

## Introduction
After a chip is taped out comes the process of bring-up and debugging, and if you can't see inside your chip, you're going to have a really bad time.

This includes generators that support system visibility (logic analyzer with memory buffer, to capture traces of what happens on your buses) and system testing (pattern generator, to insert testing data onto your system buses, allowing blocks to be tested in isolation / bad blocks to be bypassed).

### Experimental!
This is still an experimental design and the API is still subject to change.

## Usage Guide
*To be written, once the API has stabilized. Check out the examples for now.*

### Package Structure
No guarantees are made about the contents of the `examples` folder. In particular, do NOT depend on the contents of those (such as submoduled JTAG or async tools) in your designs.

## Hardware Verification
This generator has been used in these designs:
- Example design on [ICE40HX8K-B-EVN (Lattice iCE40 FPGA) through Yosys with a JTAG transport layer](examples/ice40hx8k-yosys)

Planned:
- None currently.

## TODOs

- Stabilize block I/O API