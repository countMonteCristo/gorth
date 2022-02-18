package lexer

type ByteMemory struct {
	MemorySize int
	MemPtr     int
	Data       []byte
}

func InitMemory(mem_size int) ByteMemory {
	mem := ByteMemory{
		MemorySize: mem_size,
		MemPtr:     1,
	}
	mem.Data = make([]byte, mem.MemorySize)
	return mem
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
	return
}

type Context struct {
	Memory         ByteMemory
	Allocs         map[string]int
	Consts         map[string]int
	Names          map[string]Token
	Funcs          map[string]int		// name to absolute intruction address
	StringLiterals []string
}

func InitContext(mem_size int, strlit_cap int) *Context {
	ctx := &Context{
		Memory: InitMemory(mem_size),

		Allocs:         make(map[string]int),
		Consts:         make(map[string]int),
		Names:          make(map[string]Token),
		Funcs:          make(map[string]int),
		StringLiterals: make([]string, strlit_cap),
	}

	return ctx
}
