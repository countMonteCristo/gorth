package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
	"fmt"
)

type ScopeType int

const (
	ScopeGlobal ScopeType = iota
	ScopeLocal
	ScopeUnknown
)

type Allocation struct {
	Offset types.IntType
	Size   types.IntType
}

// Allocations and constants visible only inside function
type Scope struct {
	ScopeName string
	Allocs    map[string]Allocation
	Consts    map[string]types.IntType
	Names     map[string]lexer.Token
	MemSize   types.IntType
}

const GlobalScopeName = ""

func NewScope(funcName string) *Scope {
	return &Scope{
		ScopeName: funcName, Consts: make(map[string]types.IntType), Allocs: make(map[string]Allocation),
		Names: make(map[string]lexer.Token), MemSize: 0,
	}
}

type Context struct {
	Memory ByteMemory
	Funcs  map[string]types.IntType // name to absolute intruction address

	Scopes map[string]*Scope // local function context
}

func InitContext(mem_size types.IntType) *Context {
	ctx := &Context{
		Memory: InitMemory(mem_size),
		Funcs:  make(map[string]types.IntType),
		Scopes: make(map[string]*Scope),
	}
	ctx.Scopes[GlobalScopeName] = NewScope(GlobalScopeName) // global context

	return ctx
}

func (c *Context) GlobalScope() *Scope {
	return c.Scopes[GlobalScopeName]
}

func (c *Context) GetLocalConst(name, scope_name string) (types.IntType, bool) {
	if scope_name != GlobalScopeName {
		val, exists := c.Scopes[scope_name].Consts[name]
		return val, exists
	}
	return 0, false
}
func (c *Context) GetGlobalConst(name string) (types.IntType, bool) {
	val, exists := c.GlobalScope().Consts[name]
	return val, exists
}
func (c *Context) GetConst(name, scope_name string) (types.IntType, ScopeType) {
	val, exists := c.GetLocalConst(name, scope_name)
	if exists {
		return val, ScopeLocal
	}

	val, exists = c.GetGlobalConst(name)
	if exists {
		return val, ScopeGlobal
	}
	return 0, ScopeUnknown
}

func (c *Context) GetLocalAlloc(name, scope_name string) (Allocation, bool) {
	if scope_name != GlobalScopeName {
		alloc, exists := c.Scopes[scope_name].Allocs[name]
		return alloc, exists
	}
	return Allocation{}, false
}
func (c *Context) GetGlobalAlloc(name string) (Allocation, bool) {
	alloc, exists := c.GlobalScope().Allocs[name]
	return alloc, exists
}
func (c *Context) GetAlloc(name, scope_name string) (Allocation, ScopeType) {
	alloc, exists := c.GetLocalAlloc(name, scope_name)
	if exists {
		return alloc, ScopeLocal
	}

	alloc, exists = c.GetGlobalAlloc(name)
	if exists {
		return alloc, ScopeGlobal
	}
	return Allocation{}, ScopeUnknown
}

func (c *Context) GetAllocInfo(name, scope_name string) (types.IntType, []byte) {
	alloc := c.Scopes[scope_name].Allocs[name]
	var alloc_ptr types.IntType
	if scope_name != GlobalScopeName {
		alloc_ptr = c.Memory.OperativeMemRegion.Ptr - c.Scopes[scope_name].MemSize + alloc.Offset
	} else {
		alloc_ptr = c.Memory.OperativeMemRegion.Start + alloc.Offset
	}
	alloc_mem := c.Memory.Data[alloc_ptr : alloc_ptr+alloc.Size]
	return alloc_ptr, alloc_mem
}

func (c *Context) DebugConsts(scope_name string) {
	for const_name, const_value := range c.Scopes[scope_name].Consts {
		fmt.Printf("%s=%d ", const_name, const_value)
	}
}
func (c *Context) DebugAllocs(scope_name string) {
	for alloc_name, alloc := range c.Scopes[scope_name].Allocs {
		alloc_ptr, alloc_mem := c.GetAllocInfo(alloc_name, scope_name)
		fmt.Printf("%s(%d,%d)=%d ", alloc_name, alloc_ptr, alloc.Size, alloc_mem)
	}
}

func (c *Context) DebugConstNames(names []string, scope_name string) int {
	n_found := 0
	for _, name := range names {
		const_value, exists := c.GetLocalConst(name, scope_name)
		if exists {
			fmt.Printf("(%s) %s = %d\n", scope_name, name, const_value)
			n_found++
		}
		const_value, exists = c.GetGlobalConst(name)
		if exists {
			fmt.Printf("(%s) %s = %d\n", GlobalScopeName, name, const_value)
			n_found++
		}
	}
	return n_found
}

func (c *Context) DebugAllocNames(names []string, scope_name string) int {
	n_found := 0
	for _, name := range names {
		alloc, exists := c.GetLocalAlloc(name, scope_name)
		if exists {
			alloc_ptr, alloc_mem := c.GetAllocInfo(name, scope_name)
			fmt.Printf("(%s) %s(%d,%d)=%v\n", scope_name, name, alloc_ptr, alloc.Size, alloc_mem)
			n_found++
		}
		alloc, exists = c.GetGlobalAlloc(name)
		if exists {
			alloc_ptr, alloc_mem := c.GetAllocInfo(name, GlobalScopeName)
			fmt.Printf("(%s) %s(%d,%d)=%v\n", GlobalScopeName, name, alloc_ptr, alloc.Size, alloc_mem)
			n_found++
		}
	}
	return n_found
}

func (c *Context) DebugFuncNames(names []string) int {
	n_found := 0
	for _, name := range names {
		addr, exists := c.Funcs[name]
		if exists {
			fmt.Printf("addr=%d %s\n", addr, name)
			n_found++
		}
	}
	return n_found
}
