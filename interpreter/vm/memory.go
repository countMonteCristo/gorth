package vm

import (
	"Gorth/interpreter/types"
	"fmt"
)

type MemoryRegion struct {
	Start types.IntType
	Size  types.IntType
	Ptr   types.IntType
}

// Memory for the Gorth programm represented as array of bytes.
// It is divided into several pieces:
// - String literals buffer: contains all the string literals from Gorth script, input arguments
// Strings are stored as byte arrays. When you push string into stack, you actually push
// the address and the size of the string to stack.
// - Environment - TODO: add to String literals buffer
// - Operative memory - the place where all the allocations are stored.
type ByteMemory struct {
	MemorySize types.IntType
	MemPtr     types.IntType
	Data       []byte

	Argv types.IntType // pointer to the beginning of the input arguments array

	OperativeMemRegion MemoryRegion

	StringsRegion MemoryRegion
	StringsMap    map[string]types.IntType
}

func InitMemory(mem_size types.IntType) ByteMemory {
	mem := ByteMemory{
		Data:       make([]byte, mem_size),
		MemorySize: mem_size,
		MemPtr:     1,

		StringsMap: make(map[string]types.IntType),
	}

	return mem
}

func (m *ByteMemory) Prepare(args []string) {
	for literal, addr := range m.StringsMap {
		literal_bytes := []byte(literal)
		copy(m.Data[addr:], literal_bytes)
	}

	// add input args to string literals as null-terminated strings
	m.Argv = types.IntType(m.StringsRegion.Ptr)
	for _, arg := range args {
		arg_bytes := []byte(arg)
		copy(m.Data[m.StringsRegion.Ptr:], arg_bytes)
		m.StringsRegion.Ptr += types.IntType(len(arg_bytes) + 1)
	}
	m.StringsRegion.Size = m.StringsRegion.Ptr - 1

	op_start := m.StringsRegion.Start + m.StringsRegion.Size
	m.OperativeMemRegion = MemoryRegion{
		Start: op_start,
		Size:  m.MemorySize - op_start,
		Ptr:   op_start,
	}
}

func (m *ByteMemory) LoadFromMem(ptr types.IntType, size int) (value types.IntType) {
	value = types.IntType(0)
	n := 0
	for n < size {
		value = (value << 8) | types.IntType(m.Data[ptr+types.IntType(n)])
		n++
	}
	return
}

func (m *ByteMemory) StoreToMem(ptr types.IntType, value types.IntType, size int) {
	diff := types.IntType(size - 1)
	for diff >= 0 {
		b := byte(value & 0xFF)
		m.Data[ptr+diff] = b
		value >>= 8
		diff--
	}
}

var EscapedCharToString = map[byte]string{
	'\n': "'\\n'", '\r': "'\\r'", '\t': "'\\t'", 0: "'\\0'",
}

func (m *ByteMemory) PrintDebug() {
	fmt.Printf("Allocated: %d byte(s) total\n", m.OperativeMemRegion.Ptr-m.OperativeMemRegion.Start)
	fmt.Printf("String Literals (cap=%d, size=%d):\n", m.StringsRegion.Size, m.StringsRegion.Ptr-m.StringsRegion.Start)
	data := make([]string, 0, m.StringsRegion.Ptr-m.StringsRegion.Start)
	for _, b := range m.Data[m.StringsRegion.Start:m.StringsRegion.Ptr] {
		char, exists := EscapedCharToString[b]
		if !exists {
			char = "'" + string(b) + "'"
		}
		data = append(data, char)
	}
	fmt.Printf("  %v\n", data)
	fmt.Printf(
		"Operative memory (cap=%d size=%d start=%d):\n",
		m.OperativeMemRegion.Size,
		m.OperativeMemRegion.Ptr-m.OperativeMemRegion.Start,
		m.OperativeMemRegion.Start,
	)
	fmt.Printf("  %v\n", m.Data[m.OperativeMemRegion.Start:m.OperativeMemRegion.Ptr])
}
