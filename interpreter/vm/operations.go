package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
	"strings"
)

// ---------------------------------------------------------------------------------------------------------------------

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

	OpCapture
	OpPushCaptured
	OpDropCaptures
)

// ---------------------------------------------------------------------------------------------------------------------

var OpType2Str = map[OpType]string{
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

	OpCapture:      "CAPTURE",
	OpPushCaptured: "PUSH_CAP",
	OpDropCaptures: "DROP_CAPS",
}

// ---------------------------------------------------------------------------------------------------------------------

type Op struct {
	Typ       OpType
	Operand   interface{}
	OpToken   lexer.Token
	Data      interface{}
	DebugInfo interface{}
}

// ---------------------------------------------------------------------------------------------------------------------

func (op *Op) Str(addr types.IntType) (s string) {
	var operand string

	switch op.Typ {
	case OpIntrinsic:
		operand = lexer.Intrinsic2Str[op.Operand.(lexer.IntrinsicType)]
	case OpPushInt, OpPushBool, OpPushPtr, OpCall, OpPushLocalAlloc, OpPushGlobalAlloc:
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
		operand = fmt.Sprintf("%d (%s)", res, OpJumpType2Str[op.Data.(OpJumpType)])
	case OpCapture:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.OpToken.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		type_names := make([]string, 0)
		for _, typ := range op.Data.(lexer.DataTypes) {
			type_names = append(type_names, lexer.DataType2Str[typ])
		}
		operand = fmt.Sprintf("%d (%s)", res, strings.Join(type_names, ","))
	case OpPushCaptured:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.OpToken.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		operand = fmt.Sprintf("%d (%s)", res, lexer.DataType2Str[op.Data.(lexer.DataType)])
	case OpDropCaptures:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.OpToken.Loc, "Can not cast interface to int for DropCaps: `%v`", op.Operand)
		}
		operand = fmt.Sprint(res)
	default:
		logger.VmCrash(&op.OpToken.Loc, "Unhandled operation: `%s`", OpType2Str[op.Typ])
	}

	s = fmt.Sprintf("%4d: %s %v\t\t(debug: %s)", addr, OpType2Str[op.Typ], operand, op.DebugInfo.(string))
	return
}

// ---------------------------------------------------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------------------------------------------------

var OpJumpType2Str = map[OpJumpType]string{
	OpJumpIf:       lexer.Keyword2Str[lexer.KeywordIf],
	OpJumpElse:     lexer.Keyword2Str[lexer.KeywordElse],
	OpJumpEnd:      lexer.Keyword2Str[lexer.KeywordEnd],
	OpJumpWhile:    lexer.Keyword2Str[lexer.KeywordWhile],
	OpJumpBreak:    lexer.Keyword2Str[lexer.KeywordBreak],
	OpJumpContinue: lexer.Keyword2Str[lexer.KeywordContinue],
	OpJumpReturn:   lexer.Keyword2Str[lexer.KeywordReturn],
}

var Str2OpJumpType = utils.RevMap(OpJumpType2Str).(map[string]OpJumpType)

// ---------------------------------------------------------------------------------------------------------------------
