# VT100 - A pure julia terminal emulator

[![Build Status](https://travis-ci.org/Keno/VT100.jl.svg?branch=master)](https://travis-ci.org/Keno/VT100.jl)

VT100.jl attempts to implement a small and hackable terminal emulator, mostly intended for automatic verification of Terminal based UIs. The current implementation is very simple and ignores most of the more complicated ascepts of terminal emulation, including colors, attributes and Unicode combining characters, but is nevertheless useful for UI validation in regression tests. Support for those features will be added as the need arises

# Usage

```julia
# Create an Emulator
em = VT100.Emulator()
# Feed the emulator some io
VT100.parse!(em, IOBuffer("\e[37mHello World\n"))
# Create an actual fake terminal
pty = VT100.create_pty()
# [Should pass pty.slave to C library, e.g. ncurses here]
# Now obtain a debug dump of the screen state
buf = IOBuffer()
VT100.dump(buf,devnull,em)
# buf now contains the screen contents of the emulator
```
For more examples, see the test directory.

# Usage for terminal regression tests

VT100 ships with the `test/TerminalRegressionTests.jl` collection of utilities
to simplify writing regression tests for terminal applications. The API for
this is currently in flux, but you may find it useful.
