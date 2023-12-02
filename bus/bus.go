package bus

type Bus struct {
	ram [64*1024]uint8
}

func (bus *Bus) ReadRAM(address uint16) uint8 {
	return bus.ram[address]
}

func (bus *Bus) WriteRAM(address uint16, data uint8) {
	bus.ram[address] = data
}