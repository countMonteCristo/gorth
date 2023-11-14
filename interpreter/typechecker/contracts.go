package typechecker

import (
	"Gorth/interpreter/datatypes"
	"Gorth/interpreter/intrinsics"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"
)

// ---------------------------------------------------------------------------------------------------------------------

type Contract struct {
	Inputs  *datatypes.TypeStack
	Outputs *datatypes.TypeStack
}

// ---------------------------------------------------------------------------------------------------------------------

var intrinsicType2Contract = map[intrinsics.IntrinsicType]*Contract{
	intrinsics.IntrinsicPlus: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicMinus: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicMul: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicDiv: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicMod: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},

	intrinsics.IntrinsicShl: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicShr: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},

	intrinsics.IntrinsicBitAnd: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicBitOr: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicBitXor: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicBitNot: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},

	intrinsics.IntrinsicLogicalAnd: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool, datatypes.DataTypeBool}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
	},
	intrinsics.IntrinsicLogicalOr: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool, datatypes.DataTypeBool}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
	},
	intrinsics.IntrinsicLogicalNot: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
	},

	intrinsics.IntrinsicEq: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
	},
	intrinsics.IntrinsicNe: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
	},
	intrinsics.IntrinsicLe: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
	},
	intrinsics.IntrinsicGe: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
	},
	intrinsics.IntrinsicLt: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
	},
	intrinsics.IntrinsicGt: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
	},

	intrinsics.IntrinsicDup: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny}),
	},
	intrinsics.IntrinsicSwap: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny}),
	},
	intrinsics.IntrinsicDrop: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny}),
		Outputs: utils.NewStack(datatypes.DataTypes{}),
	},
	intrinsics.IntrinsicOver: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny}),
	},
	intrinsics.IntrinsicRot: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny}),
	},

	intrinsics.IntrinsicPuti: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{}),
	},

	intrinsics.IntrinsicDebug: {
		Inputs:  utils.NewStack(datatypes.DataTypes{}),
		Outputs: utils.NewStack(datatypes.DataTypes{}),
	},
	intrinsics.IntrinsicTypeDebug: {
		Inputs:  utils.NewStack(datatypes.DataTypes{}),
		Outputs: utils.NewStack(datatypes.DataTypes{}),
	},

	intrinsics.IntrinsicAssert: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool, datatypes.DataTypePtr, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{}),
	},

	intrinsics.IntrinsicLoad8: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypePtr}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicLoad16: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypePtr}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicLoad32: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypePtr}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicLoad64: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypePtr}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},

	intrinsics.IntrinsicStore8: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypePtr}),
		Outputs: utils.NewStack(datatypes.DataTypes{}),
	},
	intrinsics.IntrinsicStore16: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypePtr}),
		Outputs: utils.NewStack(datatypes.DataTypes{}),
	},
	intrinsics.IntrinsicStore32: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypePtr}),
		Outputs: utils.NewStack(datatypes.DataTypes{}),
	},
	intrinsics.IntrinsicStore64: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypePtr}),
		Outputs: utils.NewStack(datatypes.DataTypes{}),
	},

	intrinsics.IntrinsicArgc: {
		Inputs:  utils.NewStack(datatypes.DataTypes{}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicArgv: {
		Inputs:  utils.NewStack(datatypes.DataTypes{}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypePtr}),
	},
	intrinsics.IntrinsicEnv: {
		Inputs:  utils.NewStack(datatypes.DataTypes{}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypePtr}),
	},

	intrinsics.IntrinsicSyscall0: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicSyscall1: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicSyscall2: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicSyscall3: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicSyscall4: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicSyscall5: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicSyscall6: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeAny, datatypes.DataTypeInt}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}),
	},

	intrinsics.IntrinsicCastInt: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	intrinsics.IntrinsicCastPtr: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypePtr}),
	},
	intrinsics.IntrinsicCastBool: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
	},
	intrinsics.IntrinsicCastFptr: {
		Inputs:  utils.NewStack(datatypes.DataTypes{datatypes.DataTypeAny}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeFptr}),
	},
}

// ---------------------------------------------------------------------------------------------------------------------

func GetIntrinsicContract(i intrinsics.IntrinsicType) (*Contract, string) {
	if i == intrinsics.IntrinsicOffset || i == intrinsics.IntrinsicReset {
		return nil, "compile-time intrinsics do not need contracts"
	}

	contract, ok := intrinsicType2Contract[i]
	if !ok {
		return nil, "unknown intrinsic"
	}
	return contract, ""
}

// ---------------------------------------------------------------------------------------------------------------------

type CustomFunc func() datatypes.DataTypes

var DefaultCustomFunc = func() datatypes.DataTypes { return datatypes.DataTypes{} }

// ---------------------------------------------------------------------------------------------------------------------

type IntrinsicLogicFunc func(i intrinsics.IntrinsicType, c *Contract, inputs datatypes.DataTypes, f CustomFunc) datatypes.DataTypes

func defaultLogic(i intrinsics.IntrinsicType, c *Contract, inputs datatypes.DataTypes, f CustomFunc) datatypes.DataTypes {
	return utils.StackAsSlice[datatypes.DataType](c.Outputs)
}

func GetIntrinsicLogic(i intrinsics.IntrinsicType) (IntrinsicLogicFunc, string) {
	switch i {
	case intrinsics.IntrinsicPlus, intrinsics.IntrinsicMinus, intrinsics.IntrinsicMul, intrinsics.IntrinsicDiv, intrinsics.IntrinsicMod,
		intrinsics.IntrinsicShl, intrinsics.IntrinsicShr,
		intrinsics.IntrinsicBitAnd, intrinsics.IntrinsicBitOr, intrinsics.IntrinsicBitXor, intrinsics.IntrinsicBitNot,
		intrinsics.IntrinsicLogicalAnd, intrinsics.IntrinsicLogicalOr, intrinsics.IntrinsicLogicalNot,
		intrinsics.IntrinsicEq, intrinsics.IntrinsicNe, intrinsics.IntrinsicLe, intrinsics.IntrinsicGe, intrinsics.IntrinsicLt, intrinsics.IntrinsicGt,
		intrinsics.IntrinsicPuti,
		intrinsics.IntrinsicDebug,
		intrinsics.IntrinsicAssert,
		intrinsics.IntrinsicLoad8, intrinsics.IntrinsicStore8,
		intrinsics.IntrinsicLoad16, intrinsics.IntrinsicStore16,
		intrinsics.IntrinsicLoad32, intrinsics.IntrinsicStore32,
		intrinsics.IntrinsicLoad64, intrinsics.IntrinsicStore64,
		intrinsics.IntrinsicArgc,
		intrinsics.IntrinsicArgv, intrinsics.IntrinsicEnv,
		intrinsics.IntrinsicCastInt, intrinsics.IntrinsicCastPtr, intrinsics.IntrinsicCastBool, intrinsics.IntrinsicCastFptr,
		intrinsics.IntrinsicSyscall0, intrinsics.IntrinsicSyscall1, intrinsics.IntrinsicSyscall2,
		intrinsics.IntrinsicSyscall3, intrinsics.IntrinsicSyscall4, intrinsics.IntrinsicSyscall5, intrinsics.IntrinsicSyscall6:
		return defaultLogic, ""

	case intrinsics.IntrinsicOffset, intrinsics.IntrinsicReset:
		return nil, "compile-time intrinsics do not need contracts"

	case intrinsics.IntrinsicTypeDebug:
		return func(i intrinsics.IntrinsicType, c *Contract, inputs datatypes.DataTypes, f CustomFunc) datatypes.DataTypes {
			return f()
		}, ""

	case intrinsics.IntrinsicDup:
		return func(i intrinsics.IntrinsicType, c *Contract, inputs datatypes.DataTypes, f CustomFunc) datatypes.DataTypes {
			return datatypes.DataTypes{inputs[0], inputs[0]}
		}, ""
	case intrinsics.IntrinsicSwap:
		return func(i intrinsics.IntrinsicType, c *Contract, inputs datatypes.DataTypes, f CustomFunc) datatypes.DataTypes {
			return datatypes.DataTypes{inputs[1], inputs[0]}
		}, ""
	case intrinsics.IntrinsicDrop:
		return defaultLogic, ""
	case intrinsics.IntrinsicOver:
		return func(i intrinsics.IntrinsicType, c *Contract, inputs datatypes.DataTypes, f CustomFunc) datatypes.DataTypes {
			return datatypes.DataTypes{inputs[0], inputs[1], inputs[0]}
		}, ""
	case intrinsics.IntrinsicRot:
		return func(i intrinsics.IntrinsicType, c *Contract, inputs datatypes.DataTypes, f CustomFunc) datatypes.DataTypes {
			return datatypes.DataTypes{inputs[1], inputs[2], inputs[0]}
		}, ""
	default:
		return nil, "unknown intrinsic"
	}
}

// ---------------------------------------------------------------------------------------------------------------------

var simpleOp2Contract = map[vm.OpType]*Contract{
	vm.OpPushInt: {
		Inputs:  utils.NewStack(datatypes.DataTypes{}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeInt}),
	},
	vm.OpPushBool: {
		Inputs:  utils.NewStack(datatypes.DataTypes{}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeBool}),
	},
	vm.OpPushPtr: {
		Inputs:  utils.NewStack(datatypes.DataTypes{}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypePtr}),
	},
	vm.OpPushFptr: {
		Inputs:  utils.NewStack(datatypes.DataTypes{}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypeFptr}),
	},
	vm.OpPushLocalAlloc: {
		Inputs:  utils.NewStack(datatypes.DataTypes{}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypePtr}),
	},
	vm.OpPushGlobalAlloc: {
		Inputs:  utils.NewStack(datatypes.DataTypes{}),
		Outputs: utils.NewStack(datatypes.DataTypes{datatypes.DataTypePtr}),
	},
}

func GetSimpleOpContract(o vm.OpType) (*Contract, string) {
	contract, ok := simpleOp2Contract[o]
	if !ok {
		return nil, "unknown simple operation"
	}
	return contract, ""
}

// ---------------------------------------------------------------------------------------------------------------------
