package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
)

type Allocation struct {
	Ptr types.IntType
	Size types.IntType
}


// Allocations and constants visible only inside function
type FuncContext struct {
	FuncName string
	Allocs   map[string]Allocation
	Consts   map[string]types.IntType
	Names    map[string]lexer.Token
	MemPtr   types.IntType
}

func NewFuncContext(funcName string, memPtr types.IntType) FuncContext {
	return FuncContext{
		FuncName: funcName, Consts: make(map[string]types.IntType), Allocs: make(map[string]Allocation),
		Names: make(map[string]lexer.Token), MemPtr: memPtr,
	}
}

type Context struct {
	Memory ByteMemory
	Funcs  map[string]types.IntType // name to absolute intruction address

	LocalContexts map[string]FuncContext // local function context
}

func InitContext(mem_size types.IntType) *Context {
	ctx := &Context{
		Memory:        InitMemory(mem_size),
		Funcs:         make(map[string]types.IntType),
		LocalContexts: make(map[string]FuncContext),
	}
	ctx.LocalContexts[""] = NewFuncContext("", ctx.Memory.OperativeMemRegion.Ptr) // global context

	return ctx
}

func (c *Context) GetConst(name, func_name string) (types.IntType, bool) {
	if func_name != "" {
		val, exists := c.LocalContexts[func_name].Consts[name]
		if exists {
			return val, exists
		}
	}
	val, exists := c.LocalContexts[""].Consts[name]
	if exists {
		return val, exists
	}
	return 0, false
}

func (c *Context) GetAlloc(name, func_name string) (Allocation, bool) {
	if func_name != "" {
		alloc, exists := c.LocalContexts[func_name].Allocs[name]
		if exists {
			return alloc, exists
		}
	}
	alloc, exists := c.LocalContexts[""].Allocs[name]
	if exists {
		return alloc, exists
	}
	return Allocation{}, false
}
