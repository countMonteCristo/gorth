package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
)

type OpType int

const (
	OpPushInt OpType = iota
	OpPushBool
	OpPushPtr

	OpIntrinsic

	OpJump     // jumps to current_addr + op.Operand
	OpCondJump // jumps to current_addr + op.Operand only if top value at stack is false

	OpCall
	OpFuncBegin
	OpFuncEnd

	OpPushLocalAlloc
	OpPushGlobalAlloc
)

var OpName = map[OpType]string{
	OpPushInt:  "PUSH_INT",
	OpPushBool: "PUSH_BOOL",
	OpPushPtr:  "PUSH_PTR",

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

	switch op.Typ {
	case OpIntrinsic:
		operand = lexer.IntrinsicName[op.Operand.(lexer.IntrinsicType)]
	case OpPushInt, OpPushBool, OpCall, OpPushLocalAlloc, OpPushGlobalAlloc:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.OpToken.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		operand = fmt.Sprint(res)
	case OpCondJump, OpFuncBegin, OpFuncEnd:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.OpToken.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		operand = fmt.Sprintf("%d (%s)", res, op.Data.(string))
	case OpJump:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.OpToken.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		operand = fmt.Sprintf("%d (%s)", res, OpJumpTypeName[op.Data.(OpJumpType)])
	default:
		logger.VmCrash(&op.OpToken.Loc, "Unhandled operation: `%s`", OpName[op.Typ])
	}

	s = fmt.Sprintf("%4d: %s %v\t\t(debug: %s)", addr, OpName[op.Typ], operand, op.DebugInfo.(string))
	return
}

type OpJumpType int

const (
	OpJumpIf OpJumpType = iota
	OpJumpElse
	OpJumpEnd

	OpJumpWhile

	OpJumpBreak
	OpJumpContinue
	OpJumpReturn
)

var OpJumpTypeName = map[OpJumpType]string{
	OpJumpIf:       lexer.KeywordName[lexer.KeywordIf],
	OpJumpElse:     lexer.KeywordName[lexer.KeywordElse],
	OpJumpEnd:      lexer.KeywordName[lexer.KeywordEnd],
	OpJumpWhile:    lexer.KeywordName[lexer.KeywordWhile],
	OpJumpBreak:    lexer.KeywordName[lexer.KeywordBreak],
	OpJumpContinue: lexer.KeywordName[lexer.KeywordContinue],
	OpJumpReturn:   lexer.KeywordName[lexer.KeywordReturn],
}

var NameToOpJumpType = utils.RevMap(OpJumpTypeName).(map[string]OpJumpType)
