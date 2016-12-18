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
openocd -f interface/altera-usb-blaster.cfg -c "jtag newtap x tap -irlen 3 -expected-id 0xa0123085" -c init -c "jtag_reset 0 0"
```

Connect to the OpenOCD console:
```
telnet localhost 4444
```

Load data into the pattern generator memory buffer:
```
irscan x.tap 5; drscan x.tap 8 0x5f; drscan x.tap 8 0x0a; drscan x.tap 8 0x5a;
```
This loads the pattern 0xF, 0x5, 0xA, 0x0, 0xA, 0x5

Load data into the pattern generator control line:
```
irscan x.tap 4; drscan x.tap 12 0x42a
```
(valid not bypassed, negative edge trigger, maxSample of 5, non-continuous, arm, no abort)

The LEDs should blink through the loaded samples.
