package vm

import (
	"Gorth/interpreter/types"
	"bytes"
	"encoding/binary"
	"fmt"
)

const (
	sizeof_ptr = 4
)

type MemoryRegion struct {
	Start types.IntType
	Size  types.IntType
	Ptr   types.IntType
}

// Memory for the Gorth programm represented as array of bytes.
// It is divided into several pieces:
//   - String literals buffer: contains all the string literals from Gorth script, input arguments
//     and environment variables as null-terminated strings
//   - Pointers Region: contains lists of pointers to the argv[] end env[] strings
//   - Operative Memory Reion (RAM) - this is the region where all allocations will be placed
//
// Strings are stored as null-terminated byte arrays. When you push string into stack, you actually push
// the address and the size of the string to stack.
type ByteMemory struct {
	MemorySize types.IntType
	Data       []byte

	Argv types.IntType // pointer to the null-terminated array of pointers to input arguments
	Env  types.IntType // pointer to the null-terminated array of pointers to environment variables

	StringsRegion      MemoryRegion // string literals
	PtrsRegion         MemoryRegion // pointers to argv[] and env[]
	OperativeMemRegion MemoryRegion // RAM
}

func NewMemory(mem_size types.IntType) ByteMemory {
	mem := ByteMemory{
		Data:          make([]byte, mem_size),
		MemorySize:    mem_size,
		StringsRegion: MemoryRegion{Start: types.IntType(1)},
	}

	return mem
}

func (m *ByteMemory) saveString(start types.IntType, s *string) types.IntType {
	bytes := []byte(*s)
	copy(m.Data[start:], bytes)
	return types.IntType(len(bytes) + 1)
}

func (m *ByteMemory) savePtrs(start types.IntType, ptrs *[]types.IntType) types.IntType {
	for _, ptr := range *ptrs {
		m.StoreToMem(start, ptr, sizeof_ptr)
		start += types.IntType(sizeof_ptr)
	}
	return start
}

func (m *ByteMemory) Prepare(args, env []string, strings *map[string]types.IntType) {
	ptr := m.StringsRegion.Start

	// store string literals
	for literal, addr := range *strings {
		ptr += m.saveString(addr, &literal)
	}

	// store input args
	argv_ptrs := make([]types.IntType, 0)
	for _, arg := range args {
		argv_ptrs = append(argv_ptrs, ptr)
		ptr += m.saveString(ptr, &arg)
	}
	argv_ptrs = append(argv_ptrs, 0)

	// store env variables
	env_ptrs := make([]types.IntType, 0)
	for _, e := range env {
		env_ptrs = append(env_ptrs, ptr)
		ptr += m.saveString(ptr, &e)
	}
	env_ptrs = append(env_ptrs, 0)

	// form StringsRegion
	m.StringsRegion.Size = ptr - m.StringsRegion.Size
	m.StringsRegion.Ptr = ptr

	// form PtrsRegion and save pointers to it
	m.PtrsRegion = MemoryRegion{Start: ptr, Ptr: ptr}
	m.Argv = ptr
	ptr = m.savePtrs(ptr, &argv_ptrs)
	m.Env = ptr
	ptr = m.savePtrs(ptr, &env_ptrs)
	m.PtrsRegion.Size = ptr - m.PtrsRegion.Start

	// form RAM region
	m.OperativeMemRegion = MemoryRegion{
		Start: ptr,
		Size:  m.MemorySize - ptr,
		Ptr:   ptr,
	}
}

func (m *ByteMemory) LoadFromMem(ptr types.IntType, size int) (value types.IntType) {
	sub := m.Data[ptr : ptr+types.IntType(size)]
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

	fmt.Printf("String Literals (size=%d):\n", m.StringsRegion.Size)
	data := make([]string, 0, m.StringsRegion.Ptr-m.StringsRegion.Start)
	for _, b := range m.Data[m.StringsRegion.Start:m.StringsRegion.Ptr] {
		char, exists := EscapedCharToString[b]
		if !exists {
			char = "'" + string(b) + "'"
		}
		data = append(data, char)
	}
	fmt.Printf("  %v\n", data)

	fmt.Printf("Pointers region (size=%d)\n", m.PtrsRegion.Size)
	fmt.Printf("  Argv=%d Env=%d\n", m.Argv, m.Env)
	ptrs := make([]types.IntType, 0)
	for p := m.PtrsRegion.Start; p < m.PtrsRegion.Start+m.PtrsRegion.Size; p += sizeof_ptr {
		ptrs = append(ptrs, m.LoadFromMem(p, sizeof_ptr))
	}
	fmt.Printf("  %v\n", ptrs)

	fmt.Printf(
		"Operative memory (cap=%d size=%d start=%d ptr=%d):\n",
		m.OperativeMemRegion.Size,
		m.OperativeMemRegion.Ptr-m.OperativeMemRegion.Start,
		m.OperativeMemRegion.Start,
		m.OperativeMemRegion.Ptr,
	)
	fmt.Printf("  %v\n", m.Data[m.OperativeMemRegion.Start:m.OperativeMemRegion.Ptr])
}
