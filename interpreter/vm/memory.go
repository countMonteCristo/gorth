package vm

import (
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"bytes"
	"encoding/binary"
	"fmt"
)

// ---------------------------------------------------------------------------------------------------------------------

const (
	SIZEOF_PTR = 4
)

// ---------------------------------------------------------------------------------------------------------------------

type AccessRights int

const (
	ACCESS_READ AccessRights = 1 << iota
	ACCESS_WRITE
)

// ---------------------------------------------------------------------------------------------------------------------

type MemoryRegion struct {
	Start  types.IntType
	Size   types.IntType
	Ptr    types.IntType
	Access AccessRights
}

func NewMemoryRegion(start, size types.IntType, a AccessRights) MemoryRegion {
	return MemoryRegion{Start: start, Ptr: start, Size: size, Access: a}
}

// ---------------------------------------------------------------------------------------------------------------------

func (r *MemoryRegion) HasRight(a AccessRights) bool {
	return (r.Access & a) != 0
}

// ---------------------------------------------------------------------------------------------------------------------

// Memory for the Gorth programm represented as array of bytes.
// It is divided into several pieces:
//   - String literals buffer: contains all the string literals from Gorth script, input arguments
//     and environment variables as null-terminated strings
//   - Pointers Region: contains lists of pointers to the argv[] end env[] strings
//   - Operative Memory Reion (RAM) - this is the region where all allocations will be placed
//
// Strings are stored as null-terminated byte arrays. When you push string into stack, you actually push
// the address and the size of the string to stack.
type Memory struct {
	Data []byte

	Argv types.IntType // pointer to the null-terminated array of pointers to input arguments
	Env  types.IntType // pointer to the null-terminated array of pointers to environment variables

	Strings  MemoryRegion // string literals
	Pointers MemoryRegion // pointers to argv[] and env[]
	Ram      MemoryRegion // RAM
}

func NewMemory(mem_size types.IntType) Memory {
	mem := Memory{
		Data:    make([]byte, mem_size),
		Strings: NewMemoryRegion(1, 0, ACCESS_READ),
	}

	return mem
}

// ---------------------------------------------------------------------------------------------------------------------

func (m *Memory) getMemoryRegion(ptr types.IntType, size int) []*MemoryRegion {
	result := make([]*MemoryRegion, 0)
	eptr := ptr + types.IntType(size)
	regions := []*MemoryRegion{&m.Strings, &m.Pointers, &m.Ram}
	for i, r := range regions {
		if r.Start <= ptr && ptr < r.Start+r.Size {
			result = append(result, r)
			if i < len(regions)-1 && regions[i+1].Start <= eptr && eptr < regions[i+1].Start+regions[i+1].Size {
				result = append(result, regions[i+1])
			}
			break
		}
	}
	return result
}

// ---------------------------------------------------------------------------------------------------------------------

func (m *Memory) Size() int {
	return len(m.Data)
}

// ---------------------------------------------------------------------------------------------------------------------

func (m *Memory) saveString(start types.IntType, s *string) types.IntType {
	bytes := []byte(*s)
	copy(m.Data[start:], bytes)
	return types.IntType(len(bytes) + 1)
}

func (m *Memory) savePtrs(start types.IntType, ptrs *[]types.IntType) types.IntType {
	for _, ptr := range *ptrs {
		m.StoreToMem(start, ptr, SIZEOF_PTR, nil, true)
		start += types.IntType(SIZEOF_PTR)
	}
	return start
}

func (m *Memory) Prepare(args, env []string, strings *map[string]types.IntType) {
	ptr := m.Strings.Start

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
	m.Strings.Size = ptr - m.Strings.Size
	m.Strings.Ptr = ptr

	// form PtrsRegion and save pointers to it
	m.Pointers = NewMemoryRegion(ptr, 0, ACCESS_READ)
	m.Pointers.Ptr = ptr
	m.Argv = ptr
	ptr = m.savePtrs(ptr, &argv_ptrs)
	m.Env = ptr
	ptr = m.savePtrs(ptr, &env_ptrs)
	m.Pointers.Size = ptr - m.Pointers.Start

	// form RAM region
	m.Ram = NewMemoryRegion(ptr, types.IntType(m.Size())-ptr, ACCESS_READ|ACCESS_WRITE)
}

// ---------------------------------------------------------------------------------------------------------------------

func (m *Memory) LoadFromMem(ptr types.IntType, size int, loc *utils.Location, ignore bool) (value types.IntType, err error) {
	if ptr == 0 {
		return 0, logger.VmRuntimeError(loc, "Write operation into NULL pointer")
	}

	if !ignore {
		for _, r := range m.getMemoryRegion(ptr, size) {
			if !r.HasRight(ACCESS_READ) {
				return 0, logger.VmRuntimeError(loc, "Attempt to read from protected memory region")
			}
		}
	}

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
		return 0, logger.VmRuntimeError(loc, "Cannot load value of size %d, only 1,2,4 and 8 are supported", size)
	}
	return
}

func (m *Memory) StoreToMem(ptr types.IntType, value types.IntType, size int, loc *utils.Location, ignore bool) error {
	if ptr == 0 {
		return logger.VmRuntimeError(loc, "Write operation into NULL pointer")
	}

	if !ignore {
		for _, r := range m.getMemoryRegion(ptr, size) {
			if !r.HasRight(ACCESS_WRITE) {
				return logger.VmRuntimeError(loc, "Attempt to write to protected memory region")
			}
		}
	}

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
		return logger.VmRuntimeError(loc, "Cannot store value of size %d, only 1,2,4 and 8 are supported", size)
	}
	for i, b := range buf.Bytes() {
		m.Data[ptr+types.IntType(i)] = b
	}
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

var EscapedChar2Str = map[byte]string{
	'\n': "'\\n'", '\r': "'\\r'", '\t': "'\\t'", 0: "'\\0'",
}

func (m *Memory) PrintDebug() {
	fmt.Printf("Allocated: %d byte(s) total\n", m.Ram.Ptr-m.Ram.Start)

	fmt.Printf("String Literals (size=%d):\n", m.Strings.Size)
	data := make([]string, 0, m.Strings.Ptr-m.Strings.Start)
	for _, b := range m.Data[m.Strings.Start:m.Strings.Ptr] {
		char, exists := EscapedChar2Str[b]
		if !exists {
			char = "'" + string(b) + "'"
		}
		data = append(data, char)
	}
	fmt.Printf("  %v\n", data)

	fmt.Printf("Pointers region (size=%d)\n", m.Pointers.Size)
	fmt.Printf("  Argv=%d Env=%d\n", m.Argv, m.Env)
	ptrs := make([]types.IntType, 0)
	// TODO: show error if can not load from memory
	for p := m.Pointers.Start; p < m.Pointers.Start+m.Pointers.Size; p += SIZEOF_PTR {
		val, _ := m.LoadFromMem(p, SIZEOF_PTR, nil, true)
		ptrs = append(ptrs, val)
	}
	fmt.Printf("  %v\n", ptrs)

	fmt.Printf(
		"Operative memory (cap=%d size=%d start=%d ptr=%d):\n",
		m.Ram.Size,
		m.Ram.Ptr-m.Ram.Start,
		m.Ram.Start,
		m.Ram.Ptr,
	)
	fmt.Printf("  %v\n", m.Data[m.Ram.Start:m.Ram.Ptr])
}

// ---------------------------------------------------------------------------------------------------------------------
