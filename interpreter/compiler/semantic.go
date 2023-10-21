package compiler

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"
)

// ---------------------------------------------------------------------------------------------------------------------

// Function description describes input and output arguments
type FuncSignature struct {
	Name    string
	Inputs  utils.Stack
	Outputs utils.Stack
}

// Function description - name, signature, inline flag, list of operations
type Function struct {
	Sig     FuncSignature
	Addr    types.IntType
	Inlined bool
	Ops     []vm.Op
}

// ---------------------------------------------------------------------------------------------------------------------

// Local or global memory allocations
type Allocation struct {
	Offset types.IntType // offset in memory from the beginning of all local or global allocations
	Size   types.IntType // how many bytes are allocated
}

// ---------------------------------------------------------------------------------------------------------------------

// Local or global constants defined in script
type Constant struct {
	Value types.IntType  // value which will be pushed onto the stack
	Typ   lexer.DataType // type of constant
}

// ---------------------------------------------------------------------------------------------------------------------

// Captured value
type CapturedVal struct {
	Name  string         // name to be used inside the capture block
	Typ   lexer.DataType // type of captured value
	Token *lexer.Token   // token from script where value had been captured
}

// List of all captured values in the block
type CaptureList struct {
	Vals []CapturedVal
}

func NewCaptureList() *CaptureList {
	return &CaptureList{Vals: make([]CapturedVal, 0)}
}

func (cl *CaptureList) Append(val CapturedVal) {
	cl.Vals = append(cl.Vals, val)
}

// Map for OpPushCapture operations
var DataTypeToOpType = map[lexer.DataType]vm.OpType{
	lexer.DataTypeBool: vm.OpPushBool,
	lexer.DataTypeInt:  vm.OpPushInt,
	lexer.DataTypePtr:  vm.OpPushPtr,
}

// ---------------------------------------------------------------------------------------------------------------------
