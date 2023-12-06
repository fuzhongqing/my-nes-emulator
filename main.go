package main

import (
	"github.com/fuzhongqing/my-nes-emulator/bus"
	"github.com/fuzhongqing/my-nes-emulator/cpu"
)

func main() {
	bus := &bus.Bus{}
	cpu := &cpu.MOS6502{}
	cpu.Connect(bus)

	cpu.Reset()

	cpu.Dump()
}
