package cpu

import "github.com/fuzhongqing/my-nes-emulator/bus"

//--------------------------
// instrction
//--------------------------

type Instrction struct {
	name     string
	opereta  func(*MOS6502) int
	addrmode func(*MOS6502) int
	cycles   int
}

var instrctions map[uint8]*Instrction

func init() {
	instrctions = map[uint8]*Instrction{
		0x00: {"IMP", (*MOS6502).IMP, (*MOS6502).IMP, 0},
	}
}

//--------------------------
// cpu
//--------------------------

const (
	StackAddrAbs     uint16 = 0x0100
	StackInitAddrRel uint8  = 0xFD
	PCAddrReset      uint16 = 0xFFFC
	PCAddrIRQ        uint16 = 0xFFFE
	PCAddrNMI        uint16 = 0xFFFA
)

type MOS6502 struct {
	pc       uint16
	stackPtr uint8
	acc      uint8
	x        uint8
	y        uint8
	status   uint8

	bus *bus.Bus

	fetched    uint8
	absAddr    uint16
	relAddr    uint16
	cycles     int
	clockCount int
}

func (processor *MOS6502) Connect(bus *bus.Bus) {
	processor.bus = bus
}

//--------------------------
// status section
//--------------------------

const (
	FlagCarry            uint8 = 1 << iota
	FlagZero             uint8 = 1 << iota
	FlagDisableInterrupt uint8 = 1 << iota
	FlagDecimal          uint8 = 1 << iota
	FlagBreak            uint8 = 1 << iota
	FlagUnused           uint8 = 1 << iota
	FlagOverflow         uint8 = 1 << iota
	FlagNagtive          uint8 = 1 << iota
)

func (processor *MOS6502) GetFlag(flag uint8) uint8 {
	if processor.status&flag > 0 {
		return uint8(1)
	} else {
		return uint8(0)
	}
}

func (processor *MOS6502) SetFlag(flag uint8, v bool) {
	if v {
		processor.status |= flag
	} else {
		processor.status &= ^flag
	}
}

//--------------------------
// program counter section
//--------------------------

func (p *MOS6502) ReloadProgramCounter(addr uint16) {
	p.absAddr = addr
	lo := p.Read(p.absAddr + 0)
	hi := uint16(p.Read(p.absAddr + 1))
	p.pc = (hi << 8) | uint16(lo)
}

//--------------------------
// stack section
//--------------------------

func (p *MOS6502) PushProgramCounter() {
	hi := uint8((p.pc >> 8) & 0x00FF)
	lo := uint8(p.pc & 0x00FF)

	p.Write(StackAddrAbs+uint16(p.stackPtr), hi)
	p.stackPtr--
	p.Write(StackAddrAbs+uint16(p.stackPtr), lo)
	p.stackPtr--
}

func (p *MOS6502) PushStateRegister() {
	p.SetFlag(FlagBreak, false)
	p.SetFlag(FlagUnused, true)
	p.SetFlag(FlagDisableInterrupt, true)

	p.Write(StackAddrAbs+uint16(p.stackPtr), p.status)
	p.stackPtr--
}

//--------------------------
// ram access
//--------------------------

func (processor *MOS6502) Read(address uint16) uint8 {
	return processor.bus.ReadRAM(address)
}

func (processor *MOS6502) Write(address uint16, data uint8) {
	processor.bus.WriteRAM(address, data)
}

//--------------------------
// externel
//--------------------------

func (p *MOS6502) Reset() {

	p.ReloadProgramCounter(PCAddrReset)

	// Reset internal registers
	p.acc = 0
	p.x = 0
	p.y = 0
	p.stackPtr = StackInitAddrRel
	p.status = FlagUnused

	// Clear internal helper variables
	p.absAddr = 0x0000
	p.relAddr = 0x0000
	p.fetched = 0x00

	p.cycles = 8
}

func (p *MOS6502) IRQ() {
	isDisableInterrupt := p.GetFlag(FlagDisableInterrupt) > 0

	if isDisableInterrupt {
		return
	}

	p.PushProgramCounter()
	p.PushStateRegister()
	p.ReloadProgramCounter(PCAddrIRQ)

	p.cycles = 7
}

func (p *MOS6502) NMI() {
	p.PushProgramCounter()
	p.PushStateRegister()
	p.ReloadProgramCounter(PCAddrNMI)

	p.cycles = 8
}

func (p *MOS6502) Clock() {
	if p.cycles == 0 {
		opcode := p.Read(p.pc)
		p.pc++

		instrction := instrctions[opcode]

		p.SetFlag(FlagUnused, true)

		addition_cycles_addr := instrction.addrmode(p)
		addition_cycles_op   := instrction.opereta(p)
		p.cycles = instrction.cycles + 
      (addition_cycles_addr & addition_cycles_op)

    p.SetFlag(FlagUnused, true)
	}
	p.cycles--
	p.clockCount++
}

//--------------------------
// addressing
//--------------------------

func (processor *MOS6502) IMP() int {
	// no op

	return 0
}
