package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"fmt"
)

type OpType int

const (
	OpPushInt OpType = iota
	OpPushBool
	OpIntrinsic

	OpJump		// jumps to current_addr + op.Operand
	OpCondJump	// jumps to current_addr + op.Operand only if top value at stack is false

	OpCall
	OpFuncBegin
	OpFuncEnd

	OpPushLocalAlloc
	OpPushGlobalAlloc

	OpCount = iota
)

var _ uint = OpCount - 9 // compile-time check
var OpName = map[OpType]string{
	OpPushInt:   "PUSH_INT",
	OpPushBool:  "PUSH_BOOL",
	OpIntrinsic: "INTRINSIC",

	OpJump:     "JMP",
	OpCondJump: "CJMP",

	OpCall:      "CALL",
	OpFuncBegin: "FUNC_BEGIN",
	OpFuncEnd:   "FUNC_END",

	OpPushLocalAlloc:  "PUSH_ALLOC_L",
	OpPushGlobalAlloc: "PUSH_ALLOC_G",
}

type Op struct {
	Typ       OpType
	Operand   interface{}
	OpToken   lexer.Token
	Data      interface{}
	DebugInfo interface{}
}

func (op *Op) Str(addr int64) (s string) {
	var operand string

	var _ uint = OpCount - 9 // compile-time check

	switch op.Typ {
	case OpPushBool:
		operand = types.BoolName[op.Operand.(types.BoolType)]
	case OpIntrinsic:
		operand = lexer.IntrinsicName[op.Operand.(lexer.IntrinsicType)]
	case OpPushInt, OpCall, OpPushLocalAlloc, OpPushGlobalAlloc:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.Crash(&op.OpToken.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		operand = fmt.Sprint(res)
	case OpJump, OpCondJump, OpFuncBegin, OpFuncEnd:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.Crash(&op.OpToken.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		operand = fmt.Sprintf("%d (%s)", res, op.Data.(string))
	default:
		logger.Crash(&op.OpToken.Loc, "Unhandled operation: `%s`", OpName[op.Typ])
	}

	s = fmt.Sprintf("%4d: %s %v\t\t(debug: %s)", addr, OpName[op.Typ], operand, op.DebugInfo.(string))
	return
}
