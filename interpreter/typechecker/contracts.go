package typechecker

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"
)

type Contract struct {
	Inputs  *utils.Stack
	Outputs *utils.Stack
}

// ---------------------------------------------------------------------------------------------------------------------

var intrinsicContract = map[lexer.IntrinsicType]*Contract{
	lexer.IntrinsicPlus: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicMinus: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicMul: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicDiv: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicMod: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},

	lexer.IntrinsicShl: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicShr: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},

	lexer.IntrinsicBitAnd: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicBitOr: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicBitXor: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicBitNot: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},

	lexer.IntrinsicLogicalAnd: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeBool, lexer.DataTypeBool}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
	},
	lexer.IntrinsicLogicalOr: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeBool, lexer.DataTypeBool}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
	},
	lexer.IntrinsicLogicalNot: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
	},

	lexer.IntrinsicEq: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
	},
	lexer.IntrinsicNe: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
	},
	lexer.IntrinsicLe: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
	},
	lexer.IntrinsicGe: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
	},
	lexer.IntrinsicLt: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
	},
	lexer.IntrinsicGt: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
	},

	lexer.IntrinsicDup: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeAny}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeAny, lexer.DataTypeAny}),
	},
	lexer.IntrinsicSwap: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeAny, lexer.DataTypeAny}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeAny, lexer.DataTypeAny}),
	},
	lexer.IntrinsicDrop: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeAny}),
		Outputs: utils.NewStack([]lexer.DataType{}),
	},
	lexer.IntrinsicOver: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeAny, lexer.DataTypeAny}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeAny, lexer.DataTypeAny, lexer.DataTypeAny}),
	},
	lexer.IntrinsicRot: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeAny, lexer.DataTypeAny, lexer.DataTypeAny}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeAny, lexer.DataTypeAny, lexer.DataTypeAny}),
	},

	lexer.IntrinsicPuti: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{}),
	},

	lexer.IntrinsicDebug: {
		Inputs:  utils.NewStack([]lexer.DataType{}),
		Outputs: utils.NewStack([]lexer.DataType{}),
	},
	lexer.IntrinsicTypeDebug: {
		Inputs:  utils.NewStack([]lexer.DataType{}),
		Outputs: utils.NewStack([]lexer.DataType{}),
	},

	lexer.IntrinsicLoad8: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypePtr}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicLoad16: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypePtr}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicLoad32: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypePtr}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicLoad64: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypePtr}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},

	lexer.IntrinsicStore8: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypePtr}),
		Outputs: utils.NewStack([]lexer.DataType{}),
	},
	lexer.IntrinsicStore16: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypePtr}),
		Outputs: utils.NewStack([]lexer.DataType{}),
	},
	lexer.IntrinsicStore32: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypePtr}),
		Outputs: utils.NewStack([]lexer.DataType{}),
	},
	lexer.IntrinsicStore64: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypePtr}),
		Outputs: utils.NewStack([]lexer.DataType{}),
	},

	lexer.IntrinsicArgc: {
		Inputs:  utils.NewStack([]lexer.DataType{}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicArgv: {
		Inputs:  utils.NewStack([]lexer.DataType{}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypePtr}),
	},
	lexer.IntrinsicEnv: {
		Inputs:  utils.NewStack([]lexer.DataType{}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypePtr}),
	},

	lexer.IntrinsicSyscall1: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeAny, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
	},
	lexer.IntrinsicSyscall3: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeAny, lexer.DataTypeAny, lexer.DataTypeAny, lexer.DataTypeInt}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt}),
	},

	lexer.IntrinsicCastInt: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeAny}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	lexer.IntrinsicCastPtr: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeAny}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypePtr}),
	},
	lexer.IntrinsicCastBool: {
		Inputs:  utils.NewStack([]lexer.DataType{lexer.DataTypeAny}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
	},
}

func GetIntrinsicContract(i lexer.IntrinsicType) (*Contract, string) {
	if i == lexer.IntrinsicOffset || i == lexer.IntrinsicReset {
		return nil, "compile-time intrinsics do not need contracts"
	}

	contract, ok := intrinsicContract[i]
	if !ok {
		return nil, "unknown intrinsic"
	}
	return contract, ""
}

type CustomFunc func() lexer.DataTypes

var DefaultCustomFunc = func() lexer.DataTypes { return lexer.DataTypes{} }

type IntrinsicLogicFunc func(i lexer.IntrinsicType, c *Contract, inputs lexer.DataTypes, f CustomFunc) lexer.DataTypes

func defaultLogic(i lexer.IntrinsicType, c *Contract, inputs lexer.DataTypes, f CustomFunc) lexer.DataTypes {
	return utils.StackAsSlice[lexer.DataType](c.Outputs)
}

func GetIntrinsicLogic(i lexer.IntrinsicType) (IntrinsicLogicFunc, string) {
	switch i {
	case lexer.IntrinsicPlus, lexer.IntrinsicMinus, lexer.IntrinsicMul, lexer.IntrinsicDiv, lexer.IntrinsicMod,
		lexer.IntrinsicShl, lexer.IntrinsicShr,
		lexer.IntrinsicBitAnd, lexer.IntrinsicBitOr, lexer.IntrinsicBitXor, lexer.IntrinsicBitNot,
		lexer.IntrinsicLogicalAnd, lexer.IntrinsicLogicalOr, lexer.IntrinsicLogicalNot,
		lexer.IntrinsicEq, lexer.IntrinsicNe, lexer.IntrinsicLe, lexer.IntrinsicGe, lexer.IntrinsicLt, lexer.IntrinsicGt,
		lexer.IntrinsicPuti,
		lexer.IntrinsicDebug,
		lexer.IntrinsicLoad8, lexer.IntrinsicStore8,
		lexer.IntrinsicLoad16, lexer.IntrinsicStore16,
		lexer.IntrinsicLoad32, lexer.IntrinsicStore32,
		lexer.IntrinsicLoad64, lexer.IntrinsicStore64,
		lexer.IntrinsicArgc,
		lexer.IntrinsicArgv, lexer.IntrinsicEnv,
		lexer.IntrinsicCastInt, lexer.IntrinsicCastPtr, lexer.IntrinsicCastBool,
		lexer.IntrinsicSyscall1, lexer.IntrinsicSyscall3:
		return defaultLogic, ""

	case lexer.IntrinsicOffset, lexer.IntrinsicReset:
		return nil, "compile-time intrinsics do not need contracts"

	case lexer.IntrinsicTypeDebug:
		return func(i lexer.IntrinsicType, c *Contract, inputs lexer.DataTypes, f CustomFunc) lexer.DataTypes {
			return f()
		}, ""

	case lexer.IntrinsicDup:
		return func(i lexer.IntrinsicType, c *Contract, inputs lexer.DataTypes, f CustomFunc) lexer.DataTypes {
			return lexer.DataTypes{inputs[0], inputs[0]}
		}, ""
	case lexer.IntrinsicSwap:
		return func(i lexer.IntrinsicType, c *Contract, inputs lexer.DataTypes, f CustomFunc) lexer.DataTypes {
			return lexer.DataTypes{inputs[1], inputs[0]}
		}, ""
	case lexer.IntrinsicDrop:
		return defaultLogic, ""
	case lexer.IntrinsicOver:
		return func(i lexer.IntrinsicType, c *Contract, inputs lexer.DataTypes, f CustomFunc) lexer.DataTypes {
			return lexer.DataTypes{inputs[0], inputs[1], inputs[0]}
		}, ""
	case lexer.IntrinsicRot:
		return func(i lexer.IntrinsicType, c *Contract, inputs lexer.DataTypes, f CustomFunc) lexer.DataTypes {
			return lexer.DataTypes{inputs[1], inputs[2], inputs[0]}
		}, ""
	default:
		return nil, "unknown intrinsic"
	}
}

// ---------------------------------------------------------------------------------------------------------------------

var simpleOperationContract = map[vm.OpType]*Contract{
	vm.OpPushInt:{
		Inputs:  utils.NewStack([]lexer.DataType{}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeInt}),
	},
	vm.OpPushBool:{
		Inputs:  utils.NewStack([]lexer.DataType{}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypeBool}),
	},
	vm.OpPushPtr:{
		Inputs:  utils.NewStack([]lexer.DataType{}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypePtr}),
	},
	vm.OpPushLocalAlloc:
	{
		Inputs:  utils.NewStack([]lexer.DataType{}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypePtr}),
	},
	vm.OpPushGlobalAlloc:
	{
		Inputs:  utils.NewStack([]lexer.DataType{}),
		Outputs: utils.NewStack([]lexer.DataType{lexer.DataTypePtr}),
	},
}

func GetSimpleOpContract(o vm.OpType) (*Contract, string) {
	contract, ok := simpleOperationContract[o]
	if !ok {
		return nil, "unknown simple operation"
	}
	return contract, ""
}

// ---------------------------------------------------------------------------------------------------------------------
