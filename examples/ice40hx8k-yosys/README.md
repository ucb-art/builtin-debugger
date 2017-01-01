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
Load a new blinky period (in microseconds, example below is 1/4s) to the blinky counter register (idcode 4):

```
irscan x.tap 4; drscan x.tap 24 250000;
```

### Pattern generator example
Load data into the pattern generator memory buffer (idcode 9):
```
irscan x.tap 9; drscan x.tap 12 0x05f; drscan x.tap 12 0x10a; drscan x.tap 12 0x25a; drscan x.tap 12 0x321; drscan x.tap 12 0x484;
```
This loads the pattern 0xF, 0x5, 0xA, 0x0, 0xA, 0x5, 0x1, 0x2, 0x3, 0x4

Load data into the pattern generator control line (idcode 8):
```
irscan x.tap 8; drscan x.tap 12 0x42a;
```
(ready not bypassed, negative edge trigger, maxSample of 5, non-continuous, arm, no abort) 

### Logic analyzer example

