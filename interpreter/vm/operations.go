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
	OpIf
	OpElse
	OpEnd
	OpWhile
	OpDo
	OpBreak
	OpContinue

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
	OpIf:        "IF",
	OpElse:      "ELSE",
	OpEnd:       "END",
	OpWhile:     "WHILE",
	OpDo:        "DO",
	OpBreak:     "BREAK",
	OpContinue:  "CONTINUE",

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

	// assert(OpCount == 9, "Unhandled Op in Op.str()")
	var _ uint = OpCount - 9 // compile-time check

	switch op.Typ {
	case OpPushInt:
		res, _ := op.Operand.(types.IntType)
		operand = fmt.Sprint(res)
	case OpPushBool:
		operand = types.BoolName[op.Operand.(types.BoolType)]
	case OpIntrinsic:
		operand = lexer.IntrinsicName[op.Operand.(lexer.IntrinsicType)]
	case OpIf, OpElse, OpDo, OpEnd, OpBreak, OpContinue, OpCall:
		res, _ := op.Operand.(types.IntType)
		operand = fmt.Sprint(res)
	case OpWhile:
		operand = ""
	case OpFuncBegin, OpFuncEnd:
		res, _ := op.Operand.(types.IntType)
		operand = fmt.Sprintf("%d (%s)", res, op.Data.(string))
	case OpPushLocalAlloc, OpPushGlobalAlloc:
		res, _ := op.Operand.(types.IntType)
		operand = fmt.Sprint(res)
	default:
		logger.Crash(&op.OpToken.Loc, "Unhandled operation: `%s`", OpName[op.Typ])
	}

	s = fmt.Sprintf("%4d: %s %v\t\t(debug: %s)", addr, OpName[op.Typ], operand, op.DebugInfo.(string))
	return
}
