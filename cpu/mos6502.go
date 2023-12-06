// mos 6502 cpu
// 
// https://www.nesdev.org/obelisk-6502-guide/architecture.html

package cpu

import "github.com/fuzhongqing/my-nes-emulator/bus"

//--------------------------
// instrction
//--------------------------

const (
	AddrModeIMP = iota
	AddrModeIMM = iota
	AddrModeZP0 = iota
	AddrModeZPX = iota
	AddrModeZPY = iota
	AddrModeREL = iota
	AddrModeABS = iota
	AddrModeABX = iota
	AddrModeABY = iota
	AddrModeIND = iota
	AddrModeIZX = iota
	AddrModeIZY = iota
)

type Instrction struct {
	name     string
	opereta  func(*MOS6502) int
	addrmode int
	cycles   int
}

var instrctions []*Instrction
var addrModeFuncs map[int]func(*MOS6502) int

func init() {
        // https://github.com/OneLoneCoder/olcNES/blob/ac5ce64cdb3a390a89d550c5f130682b37eeb080/Part%232%20-%20CPU/olc6502.cpp#L92
	instrctions = []*Instrction{
		{"BRK", (*MOS6502).BRK, AddrModeIMM, 7}, {"ORA", (*MOS6502).ORA, AddrModeIZX, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"???", (*MOS6502).NOP, AddrModeIMP, 3}, {"ORA", (*MOS6502).ORA, AddrModeZP0, 3}, {"ASL", (*MOS6502).ASL, AddrModeZP0, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 5}, {"PHP", (*MOS6502).PHP, AddrModeIMP, 3}, {"ORA", (*MOS6502).ORA, AddrModeIMM, 2}, {"ASL", (*MOS6502).ASL, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"ORA", (*MOS6502).ORA, AddrModeABS, 4}, {"ASL", (*MOS6502).ASL, AddrModeABS, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6},
		{"BPL", (*MOS6502).BPL, AddrModeREL, 2}, {"ORA", (*MOS6502).ORA, AddrModeIZY, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"ORA", (*MOS6502).ORA, AddrModeZPX, 4}, {"ASL", (*MOS6502).ASL, AddrModeZPX, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6}, {"CLC", (*MOS6502).CLC, AddrModeIMP, 2}, {"ORA", (*MOS6502).ORA, AddrModeABY, 4}, {"???", (*MOS6502).NOP, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 7}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"ORA", (*MOS6502).ORA, AddrModeABX, 4}, {"ASL", (*MOS6502).ASL, AddrModeABX, 7}, {"???", (*MOS6502).XXX, AddrModeIMP, 7},
		{"JSR", (*MOS6502).JSR, AddrModeABS, 6}, {"AND", (*MOS6502).AND, AddrModeIZX, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"BIT", (*MOS6502).BIT, AddrModeZP0, 3}, {"AND", (*MOS6502).AND, AddrModeZP0, 3}, {"ROL", (*MOS6502).ROL, AddrModeZP0, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 5}, {"PLP", (*MOS6502).PLP, AddrModeIMP, 4}, {"AND", (*MOS6502).AND, AddrModeIMM, 2}, {"ROL", (*MOS6502).ROL, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"BIT", (*MOS6502).BIT, AddrModeABS, 4}, {"AND", (*MOS6502).AND, AddrModeABS, 4}, {"ROL", (*MOS6502).ROL, AddrModeABS, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6},
		{"BMI", (*MOS6502).BMI, AddrModeREL, 2}, {"AND", (*MOS6502).AND, AddrModeIZY, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"AND", (*MOS6502).AND, AddrModeZPX, 4}, {"ROL", (*MOS6502).ROL, AddrModeZPX, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6}, {"SEC", (*MOS6502).SEC, AddrModeIMP, 2}, {"AND", (*MOS6502).AND, AddrModeABY, 4}, {"???", (*MOS6502).NOP, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 7}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"AND", (*MOS6502).AND, AddrModeABX, 4}, {"ROL", (*MOS6502).ROL, AddrModeABX, 7}, {"???", (*MOS6502).XXX, AddrModeIMP, 7},
		{"RTI", (*MOS6502).RTI, AddrModeIMP, 6}, {"EOR", (*MOS6502).EOR, AddrModeIZX, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"???", (*MOS6502).NOP, AddrModeIMP, 3}, {"EOR", (*MOS6502).EOR, AddrModeZP0, 3}, {"LSR", (*MOS6502).LSR, AddrModeZP0, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 5}, {"PHA", (*MOS6502).PHA, AddrModeIMP, 3}, {"EOR", (*MOS6502).EOR, AddrModeIMM, 2}, {"LSR", (*MOS6502).LSR, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"JMP", (*MOS6502).JMP, AddrModeABS, 3}, {"EOR", (*MOS6502).EOR, AddrModeABS, 4}, {"LSR", (*MOS6502).LSR, AddrModeABS, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6},
		{"BVC", (*MOS6502).BVC, AddrModeREL, 2}, {"EOR", (*MOS6502).EOR, AddrModeIZY, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"EOR", (*MOS6502).EOR, AddrModeZPX, 4}, {"LSR", (*MOS6502).LSR, AddrModeZPX, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6}, {"CLI", (*MOS6502).CLI, AddrModeIMP, 2}, {"EOR", (*MOS6502).EOR, AddrModeABY, 4}, {"???", (*MOS6502).NOP, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 7}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"EOR", (*MOS6502).EOR, AddrModeABX, 4}, {"LSR", (*MOS6502).LSR, AddrModeABX, 7}, {"???", (*MOS6502).XXX, AddrModeIMP, 7},
		{"RTS", (*MOS6502).RTS, AddrModeIMP, 6}, {"ADC", (*MOS6502).ADC, AddrModeIZX, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"???", (*MOS6502).NOP, AddrModeIMP, 3}, {"ADC", (*MOS6502).ADC, AddrModeZP0, 3}, {"ROR", (*MOS6502).ROR, AddrModeZP0, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 5}, {"PLA", (*MOS6502).PLA, AddrModeIMP, 4}, {"ADC", (*MOS6502).ADC, AddrModeIMM, 2}, {"ROR", (*MOS6502).ROR, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"JMP", (*MOS6502).JMP, AddrModeIND, 5}, {"ADC", (*MOS6502).ADC, AddrModeABS, 4}, {"ROR", (*MOS6502).ROR, AddrModeABS, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6},
		{"BVS", (*MOS6502).BVS, AddrModeREL, 2}, {"ADC", (*MOS6502).ADC, AddrModeIZY, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"ADC", (*MOS6502).ADC, AddrModeZPX, 4}, {"ROR", (*MOS6502).ROR, AddrModeZPX, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6}, {"SEI", (*MOS6502).SEI, AddrModeIMP, 2}, {"ADC", (*MOS6502).ADC, AddrModeABY, 4}, {"???", (*MOS6502).NOP, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 7}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"ADC", (*MOS6502).ADC, AddrModeABX, 4}, {"ROR", (*MOS6502).ROR, AddrModeABX, 7}, {"???", (*MOS6502).XXX, AddrModeIMP, 7},
		{"???", (*MOS6502).NOP, AddrModeIMP, 2}, {"STA", (*MOS6502).STA, AddrModeIZX, 6}, {"???", (*MOS6502).NOP, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 6}, {"STY", (*MOS6502).STY, AddrModeZP0, 3}, {"STA", (*MOS6502).STA, AddrModeZP0, 3}, {"STX", (*MOS6502).STX, AddrModeZP0, 3}, {"???", (*MOS6502).XXX, AddrModeIMP, 3}, {"DEY", (*MOS6502).DEY, AddrModeIMP, 2}, {"???", (*MOS6502).NOP, AddrModeIMP, 2}, {"TXA", (*MOS6502).TXA, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"STY", (*MOS6502).STY, AddrModeABS, 4}, {"STA", (*MOS6502).STA, AddrModeABS, 4}, {"STX", (*MOS6502).STX, AddrModeABS, 4}, {"???", (*MOS6502).XXX, AddrModeIMP, 4},
		{"BCC", (*MOS6502).BCC, AddrModeREL, 2}, {"STA", (*MOS6502).STA, AddrModeIZY, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 6}, {"STY", (*MOS6502).STY, AddrModeZPX, 4}, {"STA", (*MOS6502).STA, AddrModeZPX, 4}, {"STX", (*MOS6502).STX, AddrModeZPY, 4}, {"???", (*MOS6502).XXX, AddrModeIMP, 4}, {"TYA", (*MOS6502).TYA, AddrModeIMP, 2}, {"STA", (*MOS6502).STA, AddrModeABY, 5}, {"TXS", (*MOS6502).TXS, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 5}, {"???", (*MOS6502).NOP, AddrModeIMP, 5}, {"STA", (*MOS6502).STA, AddrModeABX, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 5},
		{"LDY", (*MOS6502).LDY, AddrModeIMM, 2}, {"LDA", (*MOS6502).LDA, AddrModeIZX, 6}, {"LDX", (*MOS6502).LDX, AddrModeIMM, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 6}, {"LDY", (*MOS6502).LDY, AddrModeZP0, 3}, {"LDA", (*MOS6502).LDA, AddrModeZP0, 3}, {"LDX", (*MOS6502).LDX, AddrModeZP0, 3}, {"???", (*MOS6502).XXX, AddrModeIMP, 3}, {"TAY", (*MOS6502).TAY, AddrModeIMP, 2}, {"LDA", (*MOS6502).LDA, AddrModeIMM, 2}, {"TAX", (*MOS6502).TAX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"LDY", (*MOS6502).LDY, AddrModeABS, 4}, {"LDA", (*MOS6502).LDA, AddrModeABS, 4}, {"LDX", (*MOS6502).LDX, AddrModeABS, 4}, {"???", (*MOS6502).XXX, AddrModeIMP, 4},
		{"BCS", (*MOS6502).BCS, AddrModeREL, 2}, {"LDA", (*MOS6502).LDA, AddrModeIZY, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 5}, {"LDY", (*MOS6502).LDY, AddrModeZPX, 4}, {"LDA", (*MOS6502).LDA, AddrModeZPX, 4}, {"LDX", (*MOS6502).LDX, AddrModeZPY, 4}, {"???", (*MOS6502).XXX, AddrModeIMP, 4}, {"CLV", (*MOS6502).CLV, AddrModeIMP, 2}, {"LDA", (*MOS6502).LDA, AddrModeABY, 4}, {"TSX", (*MOS6502).TSX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 4}, {"LDY", (*MOS6502).LDY, AddrModeABX, 4}, {"LDA", (*MOS6502).LDA, AddrModeABX, 4}, {"LDX", (*MOS6502).LDX, AddrModeABY, 4}, {"???", (*MOS6502).XXX, AddrModeIMP, 4},
		{"CPY", (*MOS6502).CPY, AddrModeIMM, 2}, {"CMP", (*MOS6502).CMP, AddrModeIZX, 6}, {"???", (*MOS6502).NOP, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"CPY", (*MOS6502).CPY, AddrModeZP0, 3}, {"CMP", (*MOS6502).CMP, AddrModeZP0, 3}, {"DEC", (*MOS6502).DEC, AddrModeZP0, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 5}, {"INY", (*MOS6502).INY, AddrModeIMP, 2}, {"CMP", (*MOS6502).CMP, AddrModeIMM, 2}, {"DEX", (*MOS6502).DEX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"CPY", (*MOS6502).CPY, AddrModeABS, 4}, {"CMP", (*MOS6502).CMP, AddrModeABS, 4}, {"DEC", (*MOS6502).DEC, AddrModeABS, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6},
		{"BNE", (*MOS6502).BNE, AddrModeREL, 2}, {"CMP", (*MOS6502).CMP, AddrModeIZY, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"CMP", (*MOS6502).CMP, AddrModeZPX, 4}, {"DEC", (*MOS6502).DEC, AddrModeZPX, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6}, {"CLD", (*MOS6502).CLD, AddrModeIMP, 2}, {"CMP", (*MOS6502).CMP, AddrModeABY, 4}, {"NOP", (*MOS6502).NOP, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 7}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"CMP", (*MOS6502).CMP, AddrModeABX, 4}, {"DEC", (*MOS6502).DEC, AddrModeABX, 7}, {"???", (*MOS6502).XXX, AddrModeIMP, 7},
		{"CPX", (*MOS6502).CPX, AddrModeIMM, 2}, {"SBC", (*MOS6502).SBC, AddrModeIZX, 6}, {"???", (*MOS6502).NOP, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"CPX", (*MOS6502).CPX, AddrModeZP0, 3}, {"SBC", (*MOS6502).SBC, AddrModeZP0, 3}, {"INC", (*MOS6502).INC, AddrModeZP0, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 5}, {"INX", (*MOS6502).INX, AddrModeIMP, 2}, {"SBC", (*MOS6502).SBC, AddrModeIMM, 2}, {"NOP", (*MOS6502).NOP, AddrModeIMP, 2}, {"???", (*MOS6502).SBC, AddrModeIMP, 2}, {"CPX", (*MOS6502).CPX, AddrModeABS, 4}, {"SBC", (*MOS6502).SBC, AddrModeABS, 4}, {"INC", (*MOS6502).INC, AddrModeABS, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6},
		{"BEQ", (*MOS6502).BEQ, AddrModeREL, 2}, {"SBC", (*MOS6502).SBC, AddrModeIZY, 5}, {"???", (*MOS6502).XXX, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 8}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"SBC", (*MOS6502).SBC, AddrModeZPX, 4}, {"INC", (*MOS6502).INC, AddrModeZPX, 6}, {"???", (*MOS6502).XXX, AddrModeIMP, 6}, {"SED", (*MOS6502).SED, AddrModeIMP, 2}, {"SBC", (*MOS6502).SBC, AddrModeABY, 4}, {"NOP", (*MOS6502).NOP, AddrModeIMP, 2}, {"???", (*MOS6502).XXX, AddrModeIMP, 7}, {"???", (*MOS6502).NOP, AddrModeIMP, 4}, {"SBC", (*MOS6502).SBC, AddrModeABX, 4}, {"INC", (*MOS6502).INC, AddrModeABX, 7}, {"???", (*MOS6502).XXX, AddrModeIMP, 7},
	}

	addrModeFuncs = map[int]func(*MOS6502) int{
		AddrModeIMP: (*MOS6502).IMP,
		AddrModeIMM: (*MOS6502).IMM,
		AddrModeZP0: (*MOS6502).ZP0,
		AddrModeZPX: (*MOS6502).ZPX,
		AddrModeZPY: (*MOS6502).ZPY,
		AddrModeREL: (*MOS6502).REL,
		AddrModeABS: (*MOS6502).ABS,
		AddrModeABX: (*MOS6502).ABX,
		AddrModeABY: (*MOS6502).ABY,
		AddrModeIND: (*MOS6502).IND,
		AddrModeIZX: (*MOS6502).IZX,
		AddrModeIZY: (*MOS6502).IZY,
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
	PCAddrBRK        uint16 = 0xFFFE
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
	opcode     uint8
	absAddr    uint16
	relAddr    uint16
	temp       uint16
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
	FlagNegative         uint8 = 1 << iota
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

func (p *MOS6502) Next() uint8 {
	data := p.Read(p.pc)
	p.pc++
	return data
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
	p.Write(StackAddrAbs+uint16(p.stackPtr), p.status)
	p.stackPtr--
}

func (p *MOS6502) PushAccRegister() {
	p.Write(StackAddrAbs+uint16(p.stackPtr), p.acc)
	p.stackPtr--
}

func (p *MOS6502) PopProgramCounter() {
	p.stackPtr++
	lo := uint16(p.Read(StackAddrAbs+uint16(p.stackPtr)))
	p.stackPtr++
	hi := uint16(p.Read(StackAddrAbs+uint16(p.stackPtr)))

	p.pc = hi << 8 + lo
}

func (p *MOS6502) PopStateRegister() {
	p.stackPtr++
	p.status = p.Read(StackAddrAbs+uint16(p.stackPtr))
}

func (p *MOS6502) PopAccRegister() {
	p.stackPtr++
	p.acc = p.Read(StackAddrAbs+uint16(p.stackPtr))
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

func (processor *MOS6502) Fetch() uint8 {

	if instrctions[processor.opcode].addrmode == AddrModeIMP {
		return 0
	}

	processor.fetched = processor.Read(processor.absAddr)
	return processor.fetched
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

	p.SetFlag(FlagBreak, false)
	p.SetFlag(FlagUnused, true)
	p.SetFlag(FlagDisableInterrupt, true)
	p.PushStateRegister()

	p.ReloadProgramCounter(PCAddrIRQ)

	p.cycles = 7
}

func (p *MOS6502) NMI() {

	p.PushProgramCounter()

	p.SetFlag(FlagBreak, false)
	p.SetFlag(FlagUnused, true)
	p.SetFlag(FlagDisableInterrupt, true)
	p.PushStateRegister()

	p.ReloadProgramCounter(PCAddrNMI)

	p.cycles = 8
}

func (p *MOS6502) Clock() {
	if p.cycles == 0 {
		p.opcode = p.Read(p.pc)
		p.pc++

		instrction := instrctions[p.opcode]

		p.SetFlag(FlagUnused, true)

		addrModeFunc := addrModeFuncs[instrction.addrmode]

		addition_cycles_addr := addrModeFunc(p)
		addition_cycles_op := instrction.opereta(p)
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
	processor.fetched = processor.acc
	return 0
}

func (p *MOS6502) IMM() int {
	p.absAddr = p.pc
	p.pc++
	return 0
}

func (p *MOS6502) ZP0() int {
	p.absAddr = uint16(p.Next())
	return 0
}

func (p *MOS6502) ZPX() int {
	p.absAddr = uint16(p.Next() + p.x)
	return 0
}

func (p *MOS6502) ZPY() int {
	p.absAddr = uint16(p.Next() + p.y)
	return 0
}

// why ?
func (p *MOS6502) REL() int {
	p.relAddr = uint16(p.Next())

	/** why is that ? */
	if (p.relAddr & 0x80) > 0 {
		p.relAddr |= 0xFF00
	}
	return 0
}

func (p *MOS6502) ABS() int {
	var lo uint16 = uint16(p.Next())
	var hi uint16 = uint16(p.Next())
	p.absAddr = (hi << 8) | lo

	return 0
}

func (p *MOS6502) ABX() int {
	var lo uint16 = uint16(p.Next())
	var hi uint16 = uint16(p.Next())
	p.absAddr = ((hi << 8) | lo) + uint16(p.x)

	if (p.absAddr & 0xFF00) != (hi << 8) {
		return 1
	}

	return 0
}

func (p *MOS6502) ABY() int {
	var lo uint16 = uint16(p.Next())
	var hi uint16 = uint16(p.Next())
	p.absAddr = ((hi << 8) | lo) + uint16(p.y)

	if (p.absAddr & 0xFF00) != (hi << 8) {
		return 1
	}

	return 0
}

func (p *MOS6502) IND() int {
	var lo uint16 = uint16(p.Next())
	var hi uint16 = uint16(p.Next())
	var ptr = hi<<8 | lo

	// todo
	if lo == 0xFF {
		p.absAddr = (uint16(p.Read(ptr&0xFF00)) << 8) | uint16(p.Read(ptr))
	} else {
		p.absAddr = (uint16(p.Read(ptr+1)) << 8) | uint16(p.Read(ptr))
	}

	return 0
}

func (p *MOS6502) IZX() int {
	var t uint16 = uint16(p.Next())

	lo := uint16(p.Read((t + uint16(p.x)) & 0x00FF))
	hi := uint16(p.Read((t + uint16(p.x) + 1) & 0x00FF))

	p.absAddr = (hi << 8) | lo

	return 0
}

func (p *MOS6502) IZY() int {

	var t = uint16(p.Next())

	lo := uint16(p.Read(t & 0x00FF))
	hi := uint16(p.Read((t + 1) & 0x00FF))

	addr_abs := (hi << 8) | lo
	addr_abs += uint16(p.y)

	p.absAddr = addr_abs

	if (p.absAddr & 0xFF00) != (hi << 8) {
		return 1
	} else {
		return 0
	}
}

//=============================
// instructions section
//=============================

//--------------------------
// data trans
//--------------------------

// load the accumulator
func (p *MOS6502) LDA() int {
	p.Fetch()
	p.acc = p.fetched

	p.SetFlag(FlagZero, p.fetched == 0x00)
	p.SetFlag(FlagNegative, p.fetched & 0x80 > 0)

	return 1
}

func (p *MOS6502) LDX() int {
	p.Fetch()
	p.x = p.fetched

	p.SetFlag(FlagZero, p.fetched == 0x00)
	p.SetFlag(FlagNegative, p.fetched & 0x80 > 0)

	return 1
}

func (p *MOS6502) LDY() int {
	p.Fetch()
	p.y = p.fetched

	p.SetFlag(FlagZero, p.fetched == 0x00)
	p.SetFlag(FlagNegative, p.fetched & 0x80 > 0)

	return 1
}

func (p *MOS6502) STA() int {
	p.Write(p.absAddr, p.acc)
	return 0
}

func (p *MOS6502) STX() int {
	p.Write(p.absAddr, p.x)
	return 0
}

func (p *MOS6502) STY() int {
	p.Write(p.absAddr, p.y)
	return 0
}

func (p *MOS6502) TAX() int {
	p.x = p.acc

	p.SetFlag(FlagZero, p.x == 0x00)
	p.SetFlag(FlagNegative, p.x & 0x80 > 0)

	return 0
}

func (p *MOS6502) TAY() int {
	p.y = p.acc

	p.SetFlag(FlagZero, p.y == 0x00)
	p.SetFlag(FlagNegative, p.y & 0x80 > 0)

	return 0
}

func (p *MOS6502) TSX() int {
	p.x = p.stackPtr

	p.SetFlag(FlagZero, p.x == 0x00)
	p.SetFlag(FlagNegative, p.x & 0x80 > 0)

	return 0
}

func (p *MOS6502) TXA() int {
	p.acc = p.x

	p.SetFlag(FlagZero, p.acc == 0x00)
	p.SetFlag(FlagNegative, p.acc & 0x80 > 0)

	return 0
}

func (p *MOS6502) TXS() int {
	p.stackPtr = p.x
	return 0
}

func (p *MOS6502) TYA() int {
	p.acc = p.y

	p.SetFlag(FlagZero, p.acc == 0x00)
	p.SetFlag(FlagNegative, p.acc & 0x80 > 0)

	return 0
}

//--------------------------
// arithmetic operations
//--------------------------

func (p *MOS6502) ADC() int {
	p.Fetch()

	p.temp = uint16(p.fetched) + uint16(p.acc) + uint16(p.GetFlag(FlagCarry))

	p.SetFlag(FlagCarry, p.temp&0xFF00 > 0)
	p.SetFlag(FlagZero, p.temp&0x00FF == 0)
	p.SetFlag(FlagOverflow, ^(uint16(p.acc)^uint16(p.fetched))&(uint16(p.acc)^uint16(p.temp))&0x0080 > 0)
	p.SetFlag(FlagNegative, p.temp & 0x0080 > 0)

	p.acc = uint8(p.temp)

	return 1
}

func (p *MOS6502) SBC() int {
	p.Fetch()

	reverse := uint16(p.fetched) ^ 0x00FF

	p.temp = uint16(p.acc) + reverse + uint16(p.GetFlag(FlagCarry))

	p.SetFlag(FlagCarry, p.temp&0xFF00 > 0)
	p.SetFlag(FlagZero, p.temp&0x00FF == 0)
	p.SetFlag(FlagOverflow, (p.temp^reverse)&(uint16(p.acc)^uint16(p.temp))&0x0080 > 0)
	p.SetFlag(FlagNegative, p.temp & 0x0080 > 0)

	p.acc = uint8(p.temp)

	return 1
}

func (p *MOS6502) INC() int {
	p.Fetch()
	p.temp = uint16(p.fetched) + 1
	p.Write(p.absAddr, uint8(p.temp))
	p.SetFlag(FlagZero, uint8(p.temp) == 0)
	p.SetFlag(FlagNegative, p.temp & 0x80 > 0)

	return 0
}

func (p *MOS6502) INX() int {
	p.x++
	p.SetFlag(FlagZero, p.x == 0)
	p.SetFlag(FlagNegative, p.x& 0x80 > 0)
	return 0
}

func (p *MOS6502) INY() int {
	p.y++
	p.SetFlag(FlagZero, p.y == 0)
	p.SetFlag(FlagNegative, p.y& 0x80 > 0)
	return 0
}

func (p *MOS6502) DEC() int {
	p.Fetch()
	p.temp = uint16(p.fetched) - 1
	p.Write(p.absAddr, uint8(p.temp))
	p.SetFlag(FlagZero, uint8(p.temp) == 0)
	p.SetFlag(FlagNegative, p.temp & 0x80 > 0)
	return 0
}

func (p *MOS6502) DEX() int {
	p.x--
	p.SetFlag(FlagZero, p.x == 0)
	p.SetFlag(FlagNegative, p.x& 0x80 > 0)
	return 0
}

func (p *MOS6502) DEY() int {
	p.y--
	p.SetFlag(FlagZero, p.y == 0)
	p.SetFlag(FlagNegative, p.y& 0x80 > 0)
	return 0
}

//--------------------------
// logic operations
//--------------------------

func (p *MOS6502) AND() int {
	p.Fetch()
	p.acc = p.acc & p.fetched
	p.SetFlag(FlagZero, p.acc == 0x00)
	p.SetFlag(FlagNegative, p.acc&0x80 > 0)
	return 1
}

func (p *MOS6502) ORA() int {
	p.Fetch()
	p.acc = p.acc | p.fetched
	p.SetFlag(FlagZero, p.acc == 0x00)
	p.SetFlag(FlagNegative, p.acc&0x80 > 0)
	return 0
}

func (p *MOS6502) EOR() int {
	p.Fetch()
	p.acc = p.acc ^ p.fetched
	p.SetFlag(FlagZero, p.acc == 0x00)
	p.SetFlag(FlagNegative, p.acc&0x80 > 0)
	return 1
}

func (p *MOS6502) BIT() int {
	p.Fetch()
	p.acc = p.acc & p.fetched
	p.SetFlag(FlagZero, p.acc == 0x00)
	p.SetFlag(FlagNegative, (p.fetched & 1<<7) > 0)
	p.SetFlag(FlagOverflow, (p.fetched & 1<<6) > 0)
	return 1
}

//--------------------------
// comparison
//--------------------------

func (p *MOS6502) CMP() int {
	p.Fetch()
	p.temp = uint16(p.acc) - uint16(p.fetched)

	p.SetFlag(FlagCarry, p.temp >= 0)
	p.SetFlag(FlagZero, p.temp&0x00FF == 0x0000)
	p.SetFlag(FlagNegative, p.temp&0x0080 > 0)

	return 1
}

func (p *MOS6502) CPX() int {
	p.Fetch()
	p.temp = uint16(p.x) - uint16(p.fetched)

	p.SetFlag(FlagCarry, p.temp >= 0)
	p.SetFlag(FlagZero, p.temp&0x00FF == 0x0000)
	p.SetFlag(FlagNegative, p.temp&0x0080 > 0)
	return 0
}

func (p *MOS6502) CPY() int {
	p.Fetch()
	p.temp = uint16(p.y) - uint16(p.fetched)

	p.SetFlag(FlagCarry, p.temp >= 0)
	p.SetFlag(FlagZero, p.temp&0x00FF == 0x0000)
	p.SetFlag(FlagNegative, p.temp&0x0080 > 0)
	return 0
}

//--------------------------
// bitwise
//--------------------------

func (p *MOS6502) ASL() int {
	p.Fetch()
	p.temp = uint16(p.fetched) << 1
	
	p.SetFlag(FlagCarry, p.temp&0xFF00 > 0)
	p.SetFlag(FlagZero, p.temp&0x00FF > 0x00)
	p.SetFlag(FlagNegative, p.temp&0x80 > 0)

	if instrctions[p.opcode].addrmode == AddrModeIMP {
		p.acc = uint8(p.temp & 0x00FF)
	} else {
		p.Write(p.absAddr, uint8(p.temp & 0x00FF))
	}

	return 0
}

func (p *MOS6502) LSR() int {
	p.Fetch()
	p.SetFlag(FlagCarry, p.fetched&0x0001 > 0)
	p.temp = uint16(p.fetched) >> 1
	p.SetFlag(FlagZero, p.temp&0x00FF > 0x00)
	p.SetFlag(FlagNegative, p.temp&0x80 > 0)

	if instrctions[p.opcode].addrmode == AddrModeIMP {
		p.acc = uint8(p.temp & 0x00FF)
	} else {
		p.Write(p.absAddr, uint8(p.temp & 0x00FF))
	}
	return 0
}

func (p *MOS6502) ROL() int {
	p.Fetch()
	p.temp = uint16(p.fetched)<<1 | uint16(p.GetFlag(FlagCarry))
	
	p.SetFlag(FlagCarry, p.temp&0xFF00 > 0)
	p.SetFlag(FlagZero, p.temp&0x00FF > 0x00)
	p.SetFlag(FlagNegative, p.temp&0x80 > 0)

	if instrctions[p.opcode].addrmode == AddrModeIMP {
		p.acc = uint8(p.temp & 0x00FF)
	} else {
		p.Write(p.absAddr, uint8(p.temp & 0x00FF))
	}
	return 0
}

func (p *MOS6502) ROR() int {
	p.Fetch()
	p.temp = uint16(p.fetched)>>1 | uint16(p.GetFlag(FlagCarry))<<7
	
	p.SetFlag(FlagCarry, p.fetched&0x01 > 0)
	p.SetFlag(FlagZero, p.temp&0x00FF > 0x00)
	p.SetFlag(FlagNegative, p.temp&0x80 > 0)

	if instrctions[p.opcode].addrmode == AddrModeIMP {
		p.acc = uint8(p.temp & 0x00FF)
	} else {
		p.Write(p.absAddr, uint8(p.temp & 0x00FF))
	}
	return 0
}

//--------------------------
// control
//--------------------------

func (p *MOS6502) JMP() int {
	p.pc = p.absAddr
	return 0
}

func (p *MOS6502) JSR() int {
	p.pc--
	p.PushProgramCounter()
	p.pc = p.absAddr
	return 0
}

func (p *MOS6502) RTS() int {
	p.PopProgramCounter()
	p.pc++
	return 0
}

func (p *MOS6502) BCC() int {
	if p.GetFlag(FlagCarry) == 0 {
		p.cycles++
		p.absAddr = p.pc + p.relAddr

		if p.absAddr&0xFF00 != p.pc&0xFF00 {
			p.cycles++
		}
		p.pc = p.absAddr
	}
	return 0
}

func (p *MOS6502) BCS() int {
	if p.GetFlag(FlagCarry) == 1 {
		p.cycles++
		p.absAddr = p.pc + p.relAddr

		if p.absAddr&0xFF00 != p.pc&0xFF00 {
			p.cycles++
		}
		p.pc = p.absAddr
	}
	return 0
}

func (p *MOS6502) BEQ() int {
	if p.GetFlag(FlagZero) == 1 {
		p.cycles++
		p.absAddr = p.pc + p.relAddr

		if p.absAddr&0xFF00 != p.pc&0xFF00 {
			p.cycles++
		}
		p.pc = p.absAddr
	}
	return 0
}

func (p *MOS6502) BMI() int {
	if p.GetFlag(FlagNegative) == 1 {
		p.cycles++
		p.absAddr = p.pc + p.relAddr

		if p.absAddr&0xFF00 != p.pc&0xFF00 {
			p.cycles++
		}
		p.pc = p.absAddr
	}
	return 0
}

func (p *MOS6502) BNE() int {
	if p.GetFlag(FlagZero) == 0 {
		p.cycles++
		p.absAddr = p.pc + p.relAddr

		if p.absAddr&0xFF00 != p.pc&0xFF00 {
			p.cycles++
		}
		p.pc = p.absAddr
	}
	return 0
}

func (p *MOS6502) BPL() int {
	if p.GetFlag(FlagNegative) == 0 {
		p.cycles++
		p.absAddr = p.pc + p.relAddr

		if p.absAddr&0xFF00 != p.pc&0xFF00 {
			p.cycles++
		}
		p.pc = p.absAddr
	}
	return 0
}

func (p *MOS6502) BVC() int {
	if p.GetFlag(FlagOverflow) == 0 {
		p.cycles++
		p.absAddr = p.pc + p.relAddr

		if p.absAddr&0xFF00 != p.pc&0xFF00 {
			p.cycles++
		}
		p.pc = p.absAddr
	}
	return 0
}

func (p *MOS6502) BVS() int {
	if p.GetFlag(FlagOverflow) == 1 {
		p.cycles++
		p.absAddr = p.pc + p.relAddr

		if p.absAddr&0xFF00 != p.pc&0xFF00 {
			p.cycles++
		}
		p.pc = p.absAddr
	}
	return 0
}

//--------------------------
// status
//--------------------------

func (p *MOS6502) CLC() int {
	p.SetFlag(FlagCarry, false)
	return 0
}

func (p *MOS6502) CLD() int {
	p.SetFlag(FlagDecimal, false)
	return 0
}

func (p *MOS6502) CLI() int {
	p.SetFlag(FlagDisableInterrupt, false)
	return 0
}

func (p *MOS6502) CLV() int {
	p.SetFlag(FlagOverflow, false)
	return 0
}

func (p *MOS6502) SEC() int {
	p.SetFlag(FlagCarry, true)
	return 0
}

func (p *MOS6502) SED() int {
	p.SetFlag(FlagDecimal, true)
	return 0
}

func (p *MOS6502) SEI() int {
	p.SetFlag(FlagDisableInterrupt, true)
	return 0
}

//--------------------------
// interrupt
//--------------------------

func (p *MOS6502) BRK() int {
	p.pc++

	p.SetFlag(FlagDisableInterrupt, true)

	p.PushProgramCounter()

	p.SetFlag(FlagBreak, true)
	p.PushStateRegister()
	p.SetFlag(FlagBreak, false)

	p.ReloadProgramCounter(PCAddrBRK)

	return 0
}

func (p *MOS6502) RTI() int {

	p.PopStateRegister()
	p.SetFlag(FlagBreak, false)
	p.SetFlag(FlagUnused, false)

	p.PopProgramCounter()

	return 0
}

//--------------------------
// stack
//--------------------------

func (p *MOS6502) PHA() int {
	p.PushAccRegister()
	return 0
}

func (p *MOS6502) PHP() int {
	p.SetFlag(FlagBreak, true)
	p.SetFlag(FlagUnused, true)
	p.PushStateRegister()

	p.SetFlag(FlagBreak, false)
	p.SetFlag(FlagUnused, false)

	return 0
}

func (p *MOS6502) PLA() int {
	p.PopAccRegister()
	p.SetFlag(FlagZero, p.acc == 0x00)
	p.SetFlag(FlagNegative, p.acc & 0x80 > 0)

	return 0
}

// pop status register off stack
func (p *MOS6502) PLP() int {
	p.PopStateRegister()
	p.SetFlag(FlagUnused, true)

	return 0
}

//--------------------------
// others
//--------------------------

func (p *MOS6502) XXX() int {
	return 0
}

func (p *MOS6502) NOP() int {
	switch p.opcode {
	case 0x1C:
	case 0x3C:
	case 0x5C:
	case 0x7C:
	case 0xDC:
	case 0xFC:
		return 1
	}

	return 0
}
