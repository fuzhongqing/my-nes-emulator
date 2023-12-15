package cpu

import (
	"os"
	"os/exec"
	"runtime"
	"fmt"
)

func ClearScreen() {
	var cmd *exec.Cmd
	if runtime.GOOS == "windows" {
		cmd = exec.Command("cmd", "/c", "cls") // Windows
	} else {
		cmd = exec.Command("clear") // Linux or MacOS
	}
	cmd.Stdout = os.Stdout
	cmd.Run()
}

// ANSI color codes
const (
	ColorReset  = "\033[0m"
	ColorRed    = "\033[31m"
	ColorGreen  = "\033[32m"
	ColorYellow = "\033[33m"
	ColorBlue   = "\033[34m"
	ColorPurple = "\033[35m"
	ColorCyan   = "\033[36m"
	ColorWhite  = "\033[37m"
)

func (cpu *MOS6502) Dump() {
	ClearScreen()

	// status
	fmt.Println("+--------+")
	fmt.Println("|CZIDBUVN|")
	fmt.Printf("|%08b|\n", cpu.status)
	fmt.Println("+--------+")

	// registers
	fmt.Printf("acc:\t%d\t0x%04x\n", cpu.acc, cpu.acc)
	fmt.Printf("x:\t%v\t0x%04x\n", cpu.x, cpu.x)
	fmt.Printf("y:\t%v\t0x%04x\n", cpu.y, cpu.y)
	fmt.Printf("pc:\t%v\t0x%04x\n", cpu.pc, cpu.pc)

	fmt.Printf("opcode:\t%v\t0x%04x\n", cpu.opcode, cpu.opcode)
	if cpu.instrction != nil {
		fmt.Printf("opname:\t%v\n", cpu.instrction.name)
	}
	fmt.Printf("data:\t%v\t0x%04x\n", cpu.fetched, cpu.fetched)

	// stack
	fmt.Println()

	spLo := uint8(0)
	if cpu.stackPtr > spLo + 5 {
		spLo = cpu.stackPtr - 5
	}
	spHi := uint8(StackInitAddrRel+1)
	if cpu.stackPtr < spHi - 5 {
		spHi = cpu.stackPtr + 5
	}

	for i := spLo; i <= spHi; i++ {
		d := cpu.Read(StackAddrAbs+uint16(i))

		if i == cpu.stackPtr {
			fmt.Printf("%v$%02x\t%v\t0x%02x%v\n", ColorPurple, i, d, d, ColorReset)
		} else {
			fmt.Printf("$%02x\t%v\t0x%02x\n", i, d, d)
		}
	}

    // counter
	fmt.Println()
	fmt.Printf("clock:\t%v\tcycles:\t%v\n", cpu.clockCount, cpu.cycles)
}
