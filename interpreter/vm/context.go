package vm

import (
	"Gorth/interpreter/lexer"
)

type Context struct {
	Memory ByteMemory
	Allocs map[string]int
	Consts map[string]int
	Names  map[string]lexer.Token
	Funcs  map[string]int // name to absolute intruction address
}

func InitContext(mem_size int) *Context {
	ctx := &Context{
		Memory: InitMemory(mem_size,),

		Allocs: make(map[string]int),
		Consts: make(map[string]int),
		Names:  make(map[string]lexer.Token),
		Funcs:  make(map[string]int),
	}

	return ctx
}
