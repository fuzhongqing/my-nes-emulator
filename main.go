package main

import (
	"github.com/fuzhongqing/my-nes-emulator/bus"
	"github.com/fuzhongqing/my-nes-emulator/cpu"
	"fmt"
)

func main() {
	bus := &bus.Bus{}
	cpu := &cpu.MOS6502{}
	cpu.Connect(bus)

	bus.WriteRAM(0xFFFC, 0x00)
	bus.WriteRAM(0xFFFD, 0x80)

	code := []uint8{ 0xA2, 
		0x0A, 
		0x8E,
		0x00,
		0x00,
		0xA2,
		0x03,
		0x8E,
		0x01,
		0x00,
		0xAC,
		0x00,
		0x00,
		0xA9,
		0x00,
		0x18,
		0x6D,
		0x01,
		0x00,
		0x88,
		0xD0,
		0xFA,
		0x8D,
		0x02,
		0x00,
		0xEA,
		0xEA,
		0xEA,
	}

	for i := range(code) {
		bus.WriteRAM(0x8000+uint16(i), code[i])
	}

	cpu.Reset()

	for {
		cpu.Clock()
		if cpu.Complete() {
			cpu.Dump()
			fmt.Scanln()
		}
	}
}
