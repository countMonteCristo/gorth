package vm

import (
	"fmt"
)

type MemoryRegion struct {
	Start int
	Size  int
	Ptr   int
}

// Memory for the Gorth programm represented as array of bytes.
// It is divided into several pieces:
// - String literals buffer: contains all the string literals from Gorth script, input arguments
// Strings are stored as byte arrays. When you push string into stack, you actually push
// the address and the size of the string to stack.
// - Environment - TODO: add to String literals buffer
// - Operative memory - the place where all the allocations are stored.
type ByteMemory struct {
	MemorySize int
	MemPtr     int
	Data       []byte

	Argv int		// pointer to the beginning of the input arguments array

	OperativeMemRegion MemoryRegion

	StringsRegion MemoryRegion
	StringsMap    map[string]int
}

func InitMemory(mem_size int) ByteMemory {
	mem := ByteMemory{
		Data:       make([]byte, mem_size),
		MemorySize: mem_size,
		MemPtr:     1,

		StringsMap: make(map[string]int),
	}

	return mem
}

func (m *ByteMemory) Prepare(args []string) {
	for literal, addr := range m.StringsMap {
		literal_bytes := []byte(literal)
		copy(m.Data[addr:], literal_bytes)
	}

	// add input args to string literals as null-terminated strings
	m.Argv = m.StringsRegion.Ptr
	for _, arg := range args {
		arg_bytes := []byte(arg)
		copy(m.Data[m.StringsRegion.Ptr:], arg_bytes)
		m.StringsRegion.Ptr += len(arg_bytes) + 1
	}
	m.StringsRegion.Size = m.StringsRegion.Ptr - 1

	op_start := m.StringsRegion.Start + m.StringsRegion.Size
	m.OperativeMemRegion = MemoryRegion{
		Start: op_start,
		Size:  m.MemorySize - op_start,
		Ptr:   op_start,
	}
}

func (m *ByteMemory) LoadFromMem(ptr int, size int) (value int) {
	value = 0
	n := 0
	for n < size {
		value = (value << 8) | int(m.Data[ptr+n])
		n++
	}
	return
}

func (m *ByteMemory) StoreToMem(ptr int, value int, size int) {
	diff := size - 1
	for diff >= 0 {
		b := byte(value & 0xFF)
		m.Data[ptr+diff] = b
		value >>= 8
		diff--
	}
}

func (m *ByteMemory) PrintDebug() {
	fmt.Printf("Totally Allocated: %d byte(s) total\n", m.MemPtr)
	fmt.Printf("NULL pointer value: %d\n", m.Data[0])
	fmt.Printf("String Literals (cap=%d, size=%d):\n", m.StringsRegion.Size, m.StringsRegion.Ptr-m.StringsRegion.Start)
	fmt.Printf("  %v\n", m.Data[m.StringsRegion.Start:m.StringsRegion.Ptr])
	fmt.Printf("Operative memory (cap=%d, size=%d):\n", m.OperativeMemRegion.Size, m.OperativeMemRegion.Ptr-m.OperativeMemRegion.Start)
	fmt.Printf("  %v\n", m.Data[m.OperativeMemRegion.Start:m.OperativeMemRegion.Ptr])
}
