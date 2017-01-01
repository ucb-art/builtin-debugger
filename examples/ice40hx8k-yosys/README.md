# builtin-debugger demo on iCE40HX8k FPGA

## Supported Hardware
Dev boards:
- iCE40HX8K-B-EVN

JTAG cables:
- Bus Blaster clone

## Preqreuisites

0. Install IceStorm: follow instructions [here](http://www.clifford.at/icestorm/)

## Build

```
sbt "run -td build"
scons
```

Optionally, use `scons prog` if you have the FPGA attached. Default is to program SRAM.

*Important*: `scons` does NOT invoke sbt or re-generate Verilog, it only builds bitfiles from generated Verilog.

## Using
Start OpenOCD and reset the JTAG TAP:
```
openocd -f interface/altera-usb-blaster.cfg -c "jtag newtap x tap -irlen 4 -expected-id 0xa0123085" -c init -c "jtag_reset 0 0"
```

Connect to the OpenOCD console:
```
telnet localhost 4444
```

### Blinky example
Load a new blinky period (in milliseconds, example below is 1/4s) to the blinky counter register (idcode 4):

```
irscan x.tap 4; drscan x.tap 16 250;
```

### Pattern generator example
Load data into the pattern generator memory buffer (idcode 9):
```
irscan x.tap 9; drscan x.tap 12 0x05f; drscan x.tap 12 0x10a; drscan x.tap 12 0x25a; drscan x.tap 12 0x321; drscan x.tap 12 0x484;
```
The pattern is 0xF, 0x5, 0xA, 0x0, 0xA, 0x5, 0x1, 0x2, 0x3, 0x4

Load data into the pattern generator control line (idcode 8):
```
irscan x.tap 8; drscan x.tap 12 0x44a;
```
(0b0 100 01001 0 1 0 = 0x44a ready not bypassed, negative edge trigger, maxSample of 9, non-continuous, arm, no abort) 

### Logic analyzer example
Load data into the logic analyzer control line (idcode 10):
```
irscan x.tap 10; drscan x.tap 12 0x422;
```
(0b0 100 001000 1 0 = 0x422 = valid not bypassed, negative edge trigger, 8 samples, arm, no abort)

Read data from the pattern generator buffer:
```
irscan x.tap 11; drscan x.tap 10 0
runtest 10; drscan x.tap 10 1
runtest 10; drscan x.tap 10 2
...
```
This scans in the address for the memory request, the response for which can be read out on the next capture. The response is of the format (last request ready, this response valid, memory line bits). The `runtest` allows time for the pattern generator to process the request and for the response to propagate back to the JTAG block.
