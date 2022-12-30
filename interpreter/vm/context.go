package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
)

type Context struct {
	Memory ByteMemory
	Allocs map[string]types.IntType
	Consts map[string]types.IntType
	Names  map[string]lexer.Token
	Funcs  map[string]types.IntType // name to absolute intruction address
}

func InitContext(mem_size types.IntType) *Context {
	ctx := &Context{
		Memory: InitMemory(mem_size),

		Allocs: make(map[string]types.IntType),
		Consts: make(map[string]types.IntType),
		Names:  make(map[string]lexer.Token),
		Funcs:  make(map[string]types.IntType),
	}

	return ctx
}
