package operations

import (
	"Gorth/interpreter/datatypes"
	"Gorth/interpreter/intrinsics"
	"Gorth/interpreter/keywords"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/tokens"
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
	OpPushFptr

	OpIntrinsic

	OpJump     // jumps to current_addr + op.Operand
	OpCondJump // jumps to current_addr + op.Operand only if top value at stack is false

	OpCall
	OpCallLike
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
	OpPushFptr: "PUSH_FPTR",

	OpIntrinsic: "INTRINSIC",

	OpJump:     "JMP",
	OpCondJump: "CJMP",

	OpCall:      "CALL",
	OpCallLike:  "CALL_LIKE",
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
	Token     *tokens.Token
	Data      interface{}
	DebugInfo interface{}
}

// ---------------------------------------------------------------------------------------------------------------------

func (op *Op) Serialize(debug bool) string {
	var operand string

	switch op.Typ {
	case OpIntrinsic:
		operand = intrinsics.Intrinsic2Str[op.Operand.(intrinsics.IntrinsicType)]
	case OpPushInt, OpPushBool, OpPushPtr, OpPushFptr, OpCall, OpPushLocalAlloc, OpPushGlobalAlloc:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.Token.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		operand = fmt.Sprint(res)
	case OpCondJump, OpFuncBegin, OpFuncEnd:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.Token.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		operand = fmt.Sprintf("%d (%s)", res, op.Data.(string))
	case OpJump:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.Token.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		operand = fmt.Sprintf("%d (%s)", res, OpJumpType2Str[op.Data.(OpJumpType)])
	case OpCapture:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.Token.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		type_names := make([]string, 0)
		for _, typ := range op.Data.(datatypes.DataTypes) {
			type_names = append(type_names, datatypes.DataType2Str[typ])
		}
		operand = fmt.Sprintf("%d (%s)", res, strings.Join(type_names, ","))
	case OpPushCaptured:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.Token.Loc, "Can not cast interface to int: `%v`", op.Operand)
		}
		operand = fmt.Sprintf("%d (%s)", res, datatypes.DataType2Str[op.Data.(datatypes.DataType)])
	case OpDropCaptures:
		res, ok := op.Operand.(types.IntType)
		if !ok {
			logger.VmCrash(&op.Token.Loc, "Can not cast interface to int for DropCaps: `%v`", op.Operand)
		}
		operand = fmt.Sprint(res)
	case OpCallLike:
		operand = fmt.Sprintf("- (%s)", op.Data.(string))
	default:
		logger.VmCrash(&op.Token.Loc, "Unhandled operation: `%s`", OpType2Str[op.Typ])
	}

	s := fmt.Sprintf("%s %v", OpType2Str[op.Typ], operand)
	if (debug) {
		s = fmt.Sprintf("%s\t\t(debug: %s)", s, op.DebugInfo.(string))
	}
	return s
}

func (op *Op) Str(addr types.IntType) string {
	return fmt.Sprintf("%4d: %s", addr, op.Serialize(true))
}

// ---------------------------------------------------------------------------------------------------------------------

type OpJumpType int

const (
	OpJumpIf OpJumpType = iota
	OpJumpElif
	OpJumpElse
	OpJumpEnd

	OpJumpWhile

	OpJumpBreak
	OpJumpContinue
	OpJumpReturn
)

// ---------------------------------------------------------------------------------------------------------------------

var OpJumpType2Str = map[OpJumpType]string{
	OpJumpIf:       keywords.Keyword2Str[keywords.KeywordIf],
	OpJumpElif:     keywords.Keyword2Str[keywords.KeywordElif],
	OpJumpElse:     keywords.Keyword2Str[keywords.KeywordElse],
	OpJumpEnd:      keywords.Keyword2Str[keywords.KeywordEnd],
	OpJumpWhile:    keywords.Keyword2Str[keywords.KeywordWhile],
	OpJumpBreak:    keywords.Keyword2Str[keywords.KeywordBreak],
	OpJumpContinue: keywords.Keyword2Str[keywords.KeywordContinue],
	OpJumpReturn:   keywords.Keyword2Str[keywords.KeywordReturn],
}

var Str2OpJumpType = utils.RevMap(OpJumpType2Str).(map[string]OpJumpType)

// ---------------------------------------------------------------------------------------------------------------------
