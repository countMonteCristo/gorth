package compiler

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"
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

type Constant struct {
	Value types.IntType
	Typ   lexer.DataType
}

var DataTypeToOpType = map[lexer.DataType]vm.OpType{
	lexer.DataTypeBool: vm.OpPushBool,
	lexer.DataTypeInt:  vm.OpPushInt,
	lexer.DataTypePtr:  vm.OpPushPtr,
}

type CapturedVal struct {
	Name  string
	Typ   lexer.DataType
	Token *lexer.Token
}

type CaptureList struct {
	Vals []CapturedVal
}

func NewCaptureList() *CaptureList {
	return &CaptureList{Vals: make([]CapturedVal, 0)}
}

func (cl *CaptureList) Append(val CapturedVal) {
	cl.Vals = append(cl.Vals, val)
}

// Allocations and constants visible only inside function
type Scope struct {
	ScopeName string
	Allocs    map[string]Allocation
	Consts    map[string]Constant
	Names     map[string]lexer.Token
	MemSize   types.IntType
	Captures  utils.Stack
}

const GlobalScopeName = ""

func NewScope(funcName string) *Scope {
	return &Scope{
		ScopeName: funcName, Consts: make(map[string]Constant), Allocs: make(map[string]Allocation),
		Names: make(map[string]lexer.Token), MemSize: 0, Captures: utils.Stack{},
	}
}

func (s *Scope) GetCapturedValue(name string) (types.IntType, bool) {
	for i, v := range s.Captures.Data {
		if v.(CapturedVal).Name == name {
			return types.IntType(s.Captures.Size() - 1 - i), true
		}
	}
	return -1, false
}

type CompileTimeContext struct {
	StringsMap           map[string]types.IntType // string literals to their memory addresses
	Funcs                map[string]Function      // function name to function
	Scopes               map[string]*Scope        // local function context (allocs, consts, etc)
	Offset               types.IntType
	CurrentFuncIsInlined bool
}

func NewCompileTimeContext() *CompileTimeContext {
	ctx := &CompileTimeContext{
		StringsMap:           make(map[string]types.IntType),
		Funcs:                make(map[string]Function),
		Scopes:               make(map[string]*Scope),
		Offset:               0,
		CurrentFuncIsInlined: false,
	}
	ctx.Scopes[GlobalScopeName] = NewScope(GlobalScopeName) // global context

	return ctx
}

func (ctx *CompileTimeContext) PreprocessStringLiterals(tokens *lexer.TokenHolder, start types.IntType) {
	address := start

	tokens.Reset()
	for !tokens.Empty() {
		token := tokens.GetNextToken()
		if token.Typ != lexer.TokenString {
			continue
		}
		literal := token.Value.(string)

		if _, exists := ctx.StringsMap[literal]; !exists {
			ctx.StringsMap[literal] = address
			address += types.IntType(len(literal) + 1) // save literals as null-terminated strings
		}
	}
}

func (c *CompileTimeContext) GlobalScope() *Scope {
	return c.Scopes[GlobalScopeName]
}

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

func (c *CompileTimeContext) GetAllocInfo(name, scope_name string, mem *vm.ByteMemory) (types.IntType, []byte) {
	alloc := c.Scopes[scope_name].Allocs[name]
	var alloc_ptr types.IntType
	if scope_name != GlobalScopeName {
		alloc_ptr = mem.OperativeMemRegion.Ptr - c.Scopes[scope_name].MemSize + alloc.Offset
	} else {
		alloc_ptr = mem.OperativeMemRegion.Start + alloc.Offset
	}
	alloc_mem := mem.Data[alloc_ptr : alloc_ptr+alloc.Size]
	return alloc_ptr, alloc_mem
}

func (c *CompileTimeContext) DebugConsts(scope_name string) {
	for const_name, constant := range c.Scopes[scope_name].Consts {
		fmt.Printf("%s=%d ", const_name, constant.Value)
	}
}
func (c *CompileTimeContext) DebugAllocs(scope_name string, mem *vm.ByteMemory) {
	for alloc_name, alloc := range c.Scopes[scope_name].Allocs {
		alloc_ptr, alloc_mem := c.GetAllocInfo(alloc_name, scope_name, mem)
		fmt.Printf("%s(%d,%d)=%d ", alloc_name, alloc_ptr, alloc.Size, alloc_mem)
	}
}

func (c *CompileTimeContext) DebugConstNames(names []string, scope_name string) int {
	n_found := 0
	for _, name := range names {
		constant, exists := c.GetLocalConst(name, scope_name)
		if exists {
			fmt.Printf("(%s) %s = %d type = %s\n", scope_name, name, constant.Value, lexer.DataTypeName[constant.Typ])
			n_found++
		}
		constant, exists = c.GetGlobalConst(name)
		if exists {
			fmt.Printf("(%s) %s = %d type = %s\n", GlobalScopeName, name, constant.Value, lexer.DataTypeName[constant.Typ])
			n_found++
		}
	}
	return n_found
}

func (c *CompileTimeContext) DebugAllocNames(names []string, scope_name string, mem *vm.ByteMemory) int {
	n_found := 0
	for _, name := range names {
		alloc, exists := c.GetLocalAlloc(name, scope_name)
		if exists {
			alloc_ptr, alloc_mem := c.GetAllocInfo(name, scope_name, mem)
			fmt.Printf("(%s) %s(%d,%d)=%v\n", scope_name, name, alloc_ptr, alloc.Size, alloc_mem)
			n_found++
		}
		alloc, exists = c.GetGlobalAlloc(name)
		if exists {
			alloc_ptr, alloc_mem := c.GetAllocInfo(name, GlobalScopeName, mem)
			fmt.Printf("(%s) %s(%d,%d)=%v\n", GlobalScopeName, name, alloc_ptr, alloc.Size, alloc_mem)
			n_found++
		}
	}
	return n_found
}

func (c *CompileTimeContext) DebugFuncNames(names []string) int {
	n_found := 0
	for _, name := range names {
		function, exists := c.Funcs[name]
		if exists {
			fmt.Printf("addr=%d %s\n", function.Addr, name)
			n_found++
		}
	}
	return n_found
}