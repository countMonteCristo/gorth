package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/utils"
	"fmt"
)

type MemoryRegion struct {
	Start int
	Size  int
	Ptr   int
}

// Memory for the Gorth programm represented as array of bytes.
// It is divided into several pieces:
// - String literals buffer: contains all the string literals from Gorth script.
// Strings are stored as byte arrays. When you push string into stack, you actually push
// the address and the size of the string to stack.
// - Input arguments buffer - not implemented yet
// - Environment - not implemented yet
// - Operative memory - the place where all the allocations are stored.
type ByteMemory struct {
	MemorySize int
	MemPtr     int
	Data       []byte

	OperativeMemRegion MemoryRegion

	StringsRegion MemoryRegion
	StringsMap    map[string]int
}

func InitMemory(mem_size, str_lit_size int) ByteMemory {
	mem := ByteMemory{
		MemorySize: mem_size + str_lit_size + 1,
		MemPtr:     1,

		StringsMap: make(map[string]int),
	}
	mem.Data = make([]byte, mem.MemorySize)
	mem.StringsRegion = mem.AllocRegion(str_lit_size)
	mem.OperativeMemRegion = mem.AllocRegion(mem_size)

	return mem
}

func (m *ByteMemory) AllocRegion(reg_size int) MemoryRegion {
	reg := MemoryRegion{
		Start: m.MemPtr,
		Size:  reg_size,
		Ptr:   m.MemPtr,
	}
	m.MemPtr += reg.Size

	return reg
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

func (m *ByteMemory) AddStringLiteral(literal string, token *lexer.Token) int {
	addr, exists := m.StringsMap[literal]
	if exists {
		return addr
	}

	new_size := m.StringsRegion.Ptr + len(literal) - m.StringsRegion.Start
	if new_size > m.StringsRegion.Size {
		lexer.CompilerFatal(
			&token.Loc, fmt.Sprintf(
				"String literals buffer size exceeded: %d > %d",
				new_size, m.StringsRegion.Size,
			))
		utils.Exit(1)
	}

	addr = m.StringsRegion.Ptr

	literal_bytes := []byte(literal)
	copy(m.Data[m.StringsRegion.Ptr:], literal_bytes)
	m.StringsRegion.Ptr += len(literal_bytes)

	m.StringsMap[literal] = addr
	return addr
}

func (m *ByteMemory) PrintDebug() {
	fmt.Printf("Totally Allocated: %d byte(s) total\n", m.MemPtr)
	fmt.Printf("NULL pointer value: %d\n", m.Data[0])
	fmt.Printf("String Literals (cap=%d, size=%d):\n", m.StringsRegion.Size, m.StringsRegion.Ptr-m.StringsRegion.Start)
	fmt.Printf("  %v\n", m.Data[m.StringsRegion.Start:m.StringsRegion.Ptr])
	fmt.Printf("Operative memory (cap=%d, size=%d):\n", m.OperativeMemRegion.Size, m.OperativeMemRegion.Ptr-m.OperativeMemRegion.Start)
	fmt.Printf("  %v\n", m.Data[m.OperativeMemRegion.Start:m.OperativeMemRegion.Ptr])
}
