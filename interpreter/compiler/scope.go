package compiler

import (
	"Gorth/interpreter/tokens"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
)

// ---------------------------------------------------------------------------------------------------------------------

type CapturedValueStack = utils.Stack[CapturedVal]

// ---------------------------------------------------------------------------------------------------------------------

type ScopeType int

const (
	ScopeGlobal ScopeType = iota
	ScopeLocal
	ScopeUnknown
)

const GlobalScopeName = ""

// ---------------------------------------------------------------------------------------------------------------------

// Could be global or local. Local scope is the scope inside of function
// Every scope has its own name, constants, allocations, captures.
// Name of the scope could be empty (global scope) or equal to the name of function
type Scope struct {
	Name     string
	Allocs   map[string]Allocation
	Consts   map[string]Constant
	Names    map[string]*tokens.Token
	MemSize  types.IntType
	Captures CapturedValueStack
}

func NewScope(funcName string) *Scope {
	return &Scope{
		Name: funcName, Consts: make(map[string]Constant), Allocs: make(map[string]Allocation),
		Names: make(map[string]*tokens.Token), MemSize: 0, Captures: CapturedValueStack{},
	}
}

// ---------------------------------------------------------------------------------------------------------------------

func (s *Scope) GetCapturedValue(name string) (types.IntType, bool) {
	for i, v := range s.Captures.Data {
		if v.Name == name {
			return types.IntType(s.Captures.Size() - 1 - i), true
		}
	}
	return -1, false
}

// ---------------------------------------------------------------------------------------------------------------------
