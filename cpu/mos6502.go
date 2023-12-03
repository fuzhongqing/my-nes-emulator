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

var instrctions map[uint8]*Instrction
var addrModeFuncs map[int]func(*MOS6502) int

func init() {
	instrctions = map[uint8]*Instrction{
		//  opc     name   func            addr         cycles
		0x00: {"IMP", (*MOS6502).IMP, AddrModeIMP, 0},
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

//--------------------------
// instruction section
//--------------------------

func (p *MOS6502) ADC() int {
	p.temp = uint16(p.fetched) + uint16(p.acc) + uint16(p.GetFlag(FlagCarry))
	overflow := (^(uint16(p.acc) ^ uint16(p.fetched)) & (uint16(p.acc) ^ uint16(p.temp)) & 0x0080) > 0

	p.SetFlag(FlagCarry, p.temp > 0xFF)
	p.SetFlag(FlagZero, (p.temp&0xFF) == 0)
	p.SetFlag(FlagOverflow, overflow)
	p.SetFlag(FlagNegative, (p.temp&0x80) > 0)

	return 1
}
