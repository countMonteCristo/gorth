package lexer

import "Gorth/interpreter/utils"

type Contract struct {
	Inputs  *utils.Stack
	Outputs *utils.Stack
}

var intrinsicContract = map[IntrinsicType]*Contract{
	IntrinsicPlus: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicMinus: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicMul: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicDiv: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicMod: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},

	IntrinsicShl: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicShr: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},

	IntrinsicBitAnd: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicBitOr: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicBitXor: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicBitNot: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},

	IntrinsicLogicalAnd: {
		Inputs:  utils.NewStack([]DataType{DataTypeBool, DataTypeBool}),
		Outputs: utils.NewStack([]DataType{DataTypeBool}),
	},
	IntrinsicLogicalOr: {
		Inputs:  utils.NewStack([]DataType{DataTypeBool, DataTypeBool}),
		Outputs: utils.NewStack([]DataType{DataTypeBool}),
	},
	IntrinsicLogicalNot: {
		Inputs:  utils.NewStack([]DataType{DataTypeBool}),
		Outputs: utils.NewStack([]DataType{DataTypeBool}),
	},

	IntrinsicEq: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeBool}),
	},
	IntrinsicNe: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeBool}),
	},
	IntrinsicLe: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeBool}),
	},
	IntrinsicGe: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeBool}),
	},
	IntrinsicLt: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeBool}),
	},
	IntrinsicGt: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeBool}),
	},

	IntrinsicDup: {
		Inputs:  utils.NewStack([]DataType{DataTypeAny}),
		Outputs: utils.NewStack([]DataType{DataTypeAny, DataTypeAny}),
	},
	IntrinsicSwap: {
		Inputs:  utils.NewStack([]DataType{DataTypeAny, DataTypeAny}),
		Outputs: utils.NewStack([]DataType{DataTypeAny, DataTypeAny}),
	},
	IntrinsicDrop: {
		Inputs:  utils.NewStack([]DataType{DataTypeAny}),
		Outputs: utils.NewStack([]DataType{}),
	},
	IntrinsicOver: {
		Inputs:  utils.NewStack([]DataType{DataTypeAny, DataTypeAny}),
		Outputs: utils.NewStack([]DataType{DataTypeAny, DataTypeAny, DataTypeAny}),
	},
	IntrinsicRot: {
		Inputs:  utils.NewStack([]DataType{DataTypeAny, DataTypeAny, DataTypeAny}),
		Outputs: utils.NewStack([]DataType{DataTypeAny, DataTypeAny, DataTypeAny}),
	},

	IntrinsicPuti: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt}),
		Outputs: utils.NewStack([]DataType{}),
	},

	IntrinsicDebug: {
		Inputs:  utils.NewStack([]DataType{}),
		Outputs: utils.NewStack([]DataType{}),
	},
	IntrinsicTypeDebug: {
		Inputs:  utils.NewStack([]DataType{}),
		Outputs: utils.NewStack([]DataType{}),
	},

	IntrinsicLoad8: {
		Inputs:  utils.NewStack([]DataType{DataTypePtr}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicLoad16: {
		Inputs:  utils.NewStack([]DataType{DataTypePtr}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicLoad32: {
		Inputs:  utils.NewStack([]DataType{DataTypePtr}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicLoad64: {
		Inputs:  utils.NewStack([]DataType{DataTypePtr}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},

	IntrinsicStore8: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypePtr}),
		Outputs: utils.NewStack([]DataType{}),
	},
	IntrinsicStore16: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypePtr}),
		Outputs: utils.NewStack([]DataType{}),
	},
	IntrinsicStore32: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypePtr}),
		Outputs: utils.NewStack([]DataType{}),
	},
	IntrinsicStore64: {
		Inputs:  utils.NewStack([]DataType{DataTypeInt, DataTypePtr}),
		Outputs: utils.NewStack([]DataType{}),
	},

	IntrinsicArgc: {
		Inputs:  utils.NewStack([]DataType{}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicArgv: {
		Inputs:  utils.NewStack([]DataType{}),
		Outputs: utils.NewStack([]DataType{DataTypePtr}),
	},
	IntrinsicEnv: {
		Inputs:  utils.NewStack([]DataType{}),
		Outputs: utils.NewStack([]DataType{DataTypePtr}),
	},

	IntrinsicSyscall1: {
		Inputs:  utils.NewStack([]DataType{DataTypeAny, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
	},
	IntrinsicSyscall3: {
		Inputs:  utils.NewStack([]DataType{DataTypeAny, DataTypeAny, DataTypeAny, DataTypeInt}),
		Outputs: utils.NewStack([]DataType{DataTypeInt, DataTypeInt}),
	},

	IntrinsicCastInt: {
		Inputs:  utils.NewStack([]DataType{DataTypeAny}),
		Outputs: utils.NewStack([]DataType{DataTypeInt}),
	},
	IntrinsicCastPtr: {
		Inputs:  utils.NewStack([]DataType{DataTypeAny}),
		Outputs: utils.NewStack([]DataType{DataTypePtr}),
	},
	IntrinsicCastBool: {
		Inputs:  utils.NewStack([]DataType{DataTypeAny}),
		Outputs: utils.NewStack([]DataType{DataTypeBool}),
	},
}

func GetIntrinsicContract(i IntrinsicType) (*Contract, string) {
	if i == IntrinsicOffset || i == IntrinsicReset {
		return nil, "compile-time intrinsics do not need contracts"
	}

	contract, ok := intrinsicContract[i]
	if !ok {
		return nil, "unknown intrinsic"
	}
	return contract, ""
}

type CustomFunc func() DataTypes

var DefaultCustomFunc = func() DataTypes { return DataTypes{} }

type IntrinsicLogicFunc func(i IntrinsicType, c *Contract, inputs DataTypes, f CustomFunc) DataTypes

func defaultLogic(i IntrinsicType, c *Contract, inputs DataTypes, f CustomFunc) DataTypes {
	return utils.StackAsSlice[DataType](c.Outputs)
}

func GetIntrinsicLogic(i IntrinsicType) (IntrinsicLogicFunc, string) {
	switch i {
	case IntrinsicPlus, IntrinsicMinus, IntrinsicMul, IntrinsicDiv, IntrinsicMod,
		IntrinsicShl, IntrinsicShr,
		IntrinsicBitAnd, IntrinsicBitOr, IntrinsicBitXor, IntrinsicBitNot,
		IntrinsicLogicalAnd, IntrinsicLogicalOr, IntrinsicLogicalNot,
		IntrinsicEq, IntrinsicNe, IntrinsicLe, IntrinsicGe, IntrinsicLt, IntrinsicGt,
		IntrinsicPuti,
		IntrinsicDebug,
		IntrinsicLoad8, IntrinsicStore8,
		IntrinsicLoad16, IntrinsicStore16,
		IntrinsicLoad32, IntrinsicStore32,
		IntrinsicLoad64, IntrinsicStore64,
		IntrinsicArgc,
		IntrinsicArgv, IntrinsicEnv,
		IntrinsicCastInt, IntrinsicCastPtr, IntrinsicCastBool,
		IntrinsicSyscall1, IntrinsicSyscall3:
		return defaultLogic, ""

	case IntrinsicOffset, IntrinsicReset:
		return nil, "compile-time intrinsics do not need contracts"

	case IntrinsicTypeDebug:
		return func(i IntrinsicType, c *Contract, inputs DataTypes, f CustomFunc) DataTypes {
			return f()
		}, ""

	case IntrinsicDup:
		return func(i IntrinsicType, c *Contract, inputs DataTypes, f CustomFunc) DataTypes {
			return DataTypes{inputs[0], inputs[0]}
		}, ""
	case IntrinsicSwap:
		return func(i IntrinsicType, c *Contract, inputs DataTypes, f CustomFunc) DataTypes {
			return DataTypes{inputs[1], inputs[0]}
		}, ""
	case IntrinsicDrop:
		return defaultLogic, ""
	case IntrinsicOver:
		return func(i IntrinsicType, c *Contract, inputs DataTypes, f CustomFunc) DataTypes {
			return DataTypes{inputs[0], inputs[1], inputs[0]}
		}, ""
	case IntrinsicRot:
		return func(i IntrinsicType, c *Contract, inputs DataTypes, f CustomFunc) DataTypes {
			return DataTypes{inputs[1], inputs[2], inputs[0]}
		}, ""
	default:
		return nil, "unknown intrinsic"
	}
}
