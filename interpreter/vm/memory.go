package vm

import (
	"Gorth/interpreter/types"
	"bytes"
	"encoding/binary"
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
	sub := m.Data[ptr:ptr+types.IntType(size)]
	buf := bytes.NewReader(sub)
	switch size {
	case 1:
		var v int8
		binary.Read(buf, binary.LittleEndian, &v)
		value = types.IntType(v)
	case 2:
		var v int16
		binary.Read(buf, binary.LittleEndian, &v)
		value = types.IntType(v)
	case 4:
		var v int32
		binary.Read(buf, binary.LittleEndian, &v)
		value = types.IntType(v)
	case 8:
		var v int64
		binary.Read(buf, binary.LittleEndian, &v)
		value = types.IntType(v)
	default:
		panic(fmt.Sprintf("Cannot load value of size %d, only 1,2,4 and 8 are supported", size))
	}
	return
}

func (m *ByteMemory) StoreToMem(ptr types.IntType, value types.IntType, size int) {
	buf := new(bytes.Buffer)
	switch size {
	case 1:
		val := int8(value)
		binary.Write(buf, binary.LittleEndian, &val)
	case 2:
		val := int16(value)
		binary.Write(buf, binary.LittleEndian, &val)
	case 4:
		val := int32(value)
		binary.Write(buf, binary.LittleEndian, &val)
	case 8:
		val := int64(value)
		binary.Write(buf, binary.LittleEndian, &val)
	default:
		panic(fmt.Sprintf("Cannot store value of size %d, only 1,2,4 and 8 are supported", size))
	}
	for i, b := range buf.Bytes() {
		m.Data[ptr+int64(i)] = b
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
		"Operative memory (cap=%d size=%d start=%d ptr=%d):\n",
		m.OperativeMemRegion.Size,
		m.OperativeMemRegion.Ptr-m.OperativeMemRegion.Start,
		m.OperativeMemRegion.Start,
		m.OperativeMemRegion.Ptr,
	)
	fmt.Printf("  %v\n", m.Data[m.OperativeMemRegion.Start:m.OperativeMemRegion.Ptr])
}
