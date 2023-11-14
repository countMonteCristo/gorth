package compiler

import (
	"Gorth/interpreter/datatypes"
	"Gorth/interpreter/tokens"
	"Gorth/interpreter/types"
	"Gorth/interpreter/vm"
	"fmt"
)

// ---------------------------------------------------------------------------------------------------------------------

// Compile time context contains all information about compiled program
type CompileTimeContext struct {
	StringsMap           map[string]types.IntType // string literals to their memory addresses
	Funcs                map[string]*Function     // function name to function
	Scopes               map[string]*Scope        // local function context (allocs, consts, etc)
	Offset               types.IntType            // current value for `offset` and `reset` keywords
	CurrentFuncIsInlined bool                     // are we compiling inline function now or not
}

func NewCompileTimeContext() *CompileTimeContext {
	ctx := &CompileTimeContext{
		StringsMap:           make(map[string]types.IntType),
		Funcs:                make(map[string]*Function),
		Scopes:               make(map[string]*Scope),
		Offset:               0,
		CurrentFuncIsInlined: false,
	}
	ctx.Scopes[GlobalScopeName] = NewScope(GlobalScopeName) // global context

	return ctx
}

// ---------------------------------------------------------------------------------------------------------------------

func (ctx *CompileTimeContext) PreprocessStringLiterals(th *tokens.TokenHolder, start types.IntType) {
	address := start

	th.Reset()
	for !th.Empty() {
		token := th.GetNextToken()
		if token.Typ != tokens.TokenString {
			continue
		}
		literal := token.Value.(string)

		if _, exists := ctx.StringsMap[literal]; !exists {
			ctx.StringsMap[literal] = address
			address += types.IntType(len(literal) + 1) // save literals as null-terminated strings
		}
	}
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *CompileTimeContext) GlobalScope() *Scope {
	return c.Scopes[GlobalScopeName]
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *CompileTimeContext) GetLocalConst(name, scope_name string) (*Constant, bool) {
	if scope_name != GlobalScopeName {
		val, exists := c.Scopes[scope_name].Consts[name]
		return &val, exists
	}
	return nil, false
}
func (c *CompileTimeContext) GetGlobalConst(name string) (*Constant, bool) {
	val, exists := c.GlobalScope().Consts[name]
	return &val, exists
}
func (c *CompileTimeContext) GetConst(name, scope_name string) (*Constant, ScopeType) {
	val, exists := c.GetLocalConst(name, scope_name)
	if exists {
		return val, ScopeLocal
	}

	val, exists = c.GetGlobalConst(name)
	if exists {
		return val, ScopeGlobal
	}
	return nil, ScopeUnknown
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *CompileTimeContext) GetLocalAlloc(name, scope_name string) (Allocation, bool) {
	if scope_name != GlobalScopeName {
		alloc, exists := c.Scopes[scope_name].Allocs[name]
		return alloc, exists
	}
	return Allocation{}, false
}
func (c *CompileTimeContext) GetGlobalAlloc(name string) (Allocation, bool) {
	alloc, exists := c.GlobalScope().Allocs[name]
	return alloc, exists
}
func (c *CompileTimeContext) GetAlloc(name, scope_name string) (Allocation, ScopeType) {
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

// ---------------------------------------------------------------------------------------------------------------------

func (c *CompileTimeContext) getAllocInfo(name, scope_name string, mem *vm.Memory) (types.IntType, []byte) {
	alloc := c.Scopes[scope_name].Allocs[name]
	var alloc_ptr types.IntType
	if scope_name != GlobalScopeName {
		alloc_ptr = mem.Ram.Ptr - alloc.Offset
	} else {
		alloc_ptr = mem.Ram.Start + alloc.Offset
	}
	alloc_mem := mem.Data[alloc_ptr : alloc_ptr+alloc.Size]
	return alloc_ptr, alloc_mem
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *CompileTimeContext) DebugConsts(scope_name string) {
	for const_name, constant := range c.Scopes[scope_name].Consts {
		fmt.Printf("%s=%d ", const_name, constant.Value)
	}
}
func (c *CompileTimeContext) DebugAllocs(scope_name string, mem *vm.Memory) {
	for alloc_name, alloc := range c.Scopes[scope_name].Allocs {
		alloc_ptr, alloc_mem := c.getAllocInfo(alloc_name, scope_name, mem)
		fmt.Printf("%s(%d,%d)=%d ", alloc_name, alloc_ptr, alloc.Size, alloc_mem)
	}
}
func (c *CompileTimeContext) DebugConstNames(names []string, scope_name string) int {
	n_found := 0
	for _, name := range names {
		if constant, exists := c.GetLocalConst(name, scope_name); exists {
			fmt.Printf("(%s) %s = %d type = %s\n", scope_name, name, constant.Value, datatypes.DataType2Str[constant.Typ])
			n_found++
		}
		if constant, exists := c.GetGlobalConst(name); exists {
			fmt.Printf("(%s) %s = %d type = %s\n", GlobalScopeName, name, constant.Value, datatypes.DataType2Str[constant.Typ])
			n_found++
		}
	}
	return n_found
}

func (c *CompileTimeContext) DebugAllocNames(names []string, scope_name string, mem *vm.Memory) int {
	n_found := 0
	for _, name := range names {
		if alloc, exists := c.GetLocalAlloc(name, scope_name); exists {
			alloc_ptr, alloc_mem := c.getAllocInfo(name, scope_name, mem)
			fmt.Printf("(%s) %s(%d,%d)=%v\n", scope_name, name, alloc_ptr, alloc.Size, alloc_mem)
			n_found++
		}

		if alloc, exists := c.GetGlobalAlloc(name); exists {
			alloc_ptr, alloc_mem := c.getAllocInfo(name, GlobalScopeName, mem)
			fmt.Printf("(%s) %s(%d,%d)=%v\n", GlobalScopeName, name, alloc_ptr, alloc.Size, alloc_mem)
			n_found++
		}
	}
	return n_found
}
func (c *CompileTimeContext) DebugFuncNames(names []string) int {
	n_found := 0
	for _, name := range names {
		if function, exists := c.Funcs[name]; exists {
			if function.Inlined {
				fmt.Printf("(inlined) %s\n", name)
			} else {
				fmt.Printf("addr=%d %s\n", function.Addr, name)
			}

			n_found++
		}
	}
	return n_found
}

// TODO: Debug captures

// ---------------------------------------------------------------------------------------------------------------------
