package vm

import (
	"GoStudy/Gorth/interpreter/lexer"
	"GoStudy/Gorth/interpreter/utils"
	"fmt"
)

type GorthStr struct {
	Size int
	Ptr  int
}

type MemoryRegion struct {
	Start int
	Size  int
	Ptr   int
}

// Memory for the Gorth programm represented as array of bytes.
// It is divided into several pieces:
// - String literals buffer: contains all the string literals from Gorth script.
// Size of each string literal and pointer to the string characters are stored
// in StringLitsBuffer array. The index of each string literal is stored in
// OpPushStr operand.
// - Input arguments buffer - not implemented yet
// - Environment - not implemented yet
// - Operative memory - the place where all the allocations are stored.
type ByteMemory struct {
	MemorySize int
	MemPtr     int
	Data       []byte

	OperativeMemRegion MemoryRegion

	StringLitsRegion MemoryRegion
	StringLitsBuffer []GorthStr
	StringLitsMap    map[string]int
}

func InitMemory(mem_size, str_lit_size int) ByteMemory {
	mem := ByteMemory{
		MemorySize: mem_size + str_lit_size + 1,
		MemPtr:     1,

		StringLitsBuffer: make([]GorthStr, 0),
		StringLitsMap:    make(map[string]int),
	}
	mem.Data = make([]byte, mem.MemorySize)
	mem.StringLitsRegion = mem.AllocRegion(str_lit_size)
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

func (m *ByteMemory) AddStringLit(str string, token *lexer.Token) int {
	index, exists := m.StringLitsMap[str]
	if exists {
		return index
	}

	start := m.StringLitsRegion.Ptr

	new_size := m.StringLitsRegion.Ptr + len(str) - m.StringLitsRegion.Start
	if new_size > m.StringLitsRegion.Size {
		lexer.CompilerFatal(
			&token.Loc, fmt.Sprintf(
				"String literals buffer size exceeded: %d > %d",
				new_size, m.StringLitsRegion.Size,
			))
		utils.Exit(1)
	}

	copy(m.Data[m.StringLitsRegion.Ptr:], str)
	m.StringLitsRegion.Ptr += len(str)

	gstr := GorthStr{
		Size: len(str),
		Ptr:  start,
	}
	index = len(m.StringLitsBuffer)
	m.StringLitsBuffer = append(m.StringLitsBuffer, gstr)
	m.StringLitsMap[str] = index
	return index
}

func (m *ByteMemory) PrintDebug() {
	fmt.Printf("Totally Allocated: %d byte(s) total\n", m.MemPtr)
	fmt.Printf("NULL pointer value: %d\n", m.Data[0])
	fmt.Printf("String Literals (cap=%d, size=%d):\n", m.StringLitsRegion.Size, m.StringLitsRegion.Ptr-m.StringLitsRegion.Start)
	fmt.Printf("  %v\n", m.Data[m.StringLitsRegion.Start:m.StringLitsRegion.Ptr])
	fmt.Printf("Operative memory (cap=%d, size=%d):\n", m.OperativeMemRegion.Size, m.OperativeMemRegion.Ptr-m.OperativeMemRegion.Start)
	fmt.Printf("  %v\n", m.Data[m.OperativeMemRegion.Start:m.OperativeMemRegion.Ptr])
}

type Context struct {
	Memory ByteMemory
	Allocs map[string]int
	Consts map[string]int
	Names  map[string]lexer.Token
	Funcs  map[string]int // name to absolute intruction address
}

func InitContext(mem_size int, strlit_size int) *Context {
	ctx := &Context{
		Memory: InitMemory(mem_size, strlit_size),

		Allocs: make(map[string]int),
		Consts: make(map[string]int),
		Names:  make(map[string]lexer.Token),
		Funcs:  make(map[string]int),
	}

	return ctx
}
