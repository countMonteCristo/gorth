package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
	"fmt"
	"strconv"
)

type OpType int

const (
	OpPushInt   OpType = iota
	OpPushBool  OpType = iota
	OpIntrinsic OpType = iota
	OpIf        OpType = iota
	OpElse      OpType = iota
	OpEnd       OpType = iota
	OpWhile     OpType = iota
	OpDo        OpType = iota
	OpBreak     OpType = iota
	OpContinue  OpType = iota

	OpCall      OpType = iota
	OpFuncBegin OpType = iota
	OpFuncEnd   OpType = iota

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
}

type Op struct {
	Typ     OpType
	Operand interface{}
	OpToken lexer.Token
}

func (op *Op) Str(addr int) (s string) {
	var operand string

	// assert(OpCount == 9, "Unhandled Op in Op.str()")
	var _ uint = OpCount - 9 // compile-time check

	switch op.Typ {
	case OpPushInt:
		operand = strconv.Itoa(op.Operand.(int))
	case OpPushBool:
		operand = types.BoolName[op.Operand.(types.BoolType)]
	case OpIntrinsic:
		operand = lexer.IntrinsicName[op.Operand.(lexer.IntrinsicType)]
	case OpIf:
		operand = strconv.Itoa(op.Operand.(int))
	case OpElse:
		operand = strconv.Itoa(op.Operand.(int))
	case OpWhile:
		operand = ""
	case OpDo:
		operand = strconv.Itoa(op.Operand.(int))
	case OpEnd:
		operand = strconv.Itoa(op.Operand.(int))
	case OpBreak:
		operand = strconv.Itoa(op.Operand.(int))
	case OpContinue:
		operand = strconv.Itoa(op.Operand.(int))
	case OpFuncBegin:
		operand = op.Operand.(string)
	case OpFuncEnd:
		operand = op.Operand.(string)
	case OpCall:
		operand = strconv.Itoa(op.Operand.(int))
	default:
		lexer.CompilerFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled operation: `%s`", OpName[op.Typ]))
	}

	s = fmt.Sprintf("%4d: %s %v", addr, OpName[op.Typ], operand)
	return
}
