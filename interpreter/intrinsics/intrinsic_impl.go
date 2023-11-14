package intrinsics

import (
	"Gorth/interpreter/types"
)

// ---------------------------------------------------------------------------------------------------------------------

type Binary[T any] func(x, y T) T

// ---------------------------------------------------------------------------------------------------------------------

func B2I(x bool) types.BoolType {
	if x {
		return types.BoolTrue
	}
	return types.BoolFalse
}

func I2B(x types.BoolType) bool {
	return x != 0
}

// ---------------------------------------------------------------------------------------------------------------------

var SafeArithmeticFunctions = map[IntrinsicType]Binary[types.IntType]{
	IntrinsicPlus:   func(x, y types.IntType) types.IntType { return x + y },
	IntrinsicMinus:  func(x, y types.IntType) types.IntType { return x - y },
	IntrinsicMul:    func(x, y types.IntType) types.IntType { return x * y },
	IntrinsicBitAnd: func(x, y types.IntType) types.IntType { return x & y },
	IntrinsicBitOr:  func(x, y types.IntType) types.IntType { return x | y },
	IntrinsicBitXor: func(x, y types.IntType) types.IntType { return x ^ y },
}

// ---------------------------------------------------------------------------------------------------------------------

var LogicalFunctions = map[IntrinsicType]Binary[types.BoolType]{
	IntrinsicLogicalAnd: func(x, y types.BoolType) types.BoolType { return B2I(I2B(x) && I2B(y)) },
	IntrinsicLogicalOr:  func(x, y types.BoolType) types.BoolType { return B2I(I2B(x) || I2B(y)) },
}

// ---------------------------------------------------------------------------------------------------------------------

var ComparableFunctions = map[IntrinsicType]func(x, y types.IntType) types.BoolType{
	IntrinsicEq: func(x, y types.IntType) types.BoolType { return B2I(x == y) },
	IntrinsicNe: func(x, y types.IntType) types.BoolType { return B2I(x != y) },
	IntrinsicLe: func(x, y types.IntType) types.BoolType { return B2I(x <= y) },
	IntrinsicGe: func(x, y types.IntType) types.BoolType { return B2I(x >= y) },
	IntrinsicLt: func(x, y types.IntType) types.BoolType { return B2I(x < y) },
	IntrinsicGt: func(x, y types.IntType) types.BoolType { return B2I(x > y) },
}

// ---------------------------------------------------------------------------------------------------------------------

var LoadSizes = map[IntrinsicType]int{
	IntrinsicLoad8: 1, IntrinsicLoad16: 2,
	IntrinsicLoad32: 4, IntrinsicLoad64: 8,
}

var StoreSizes = map[IntrinsicType]int{
	IntrinsicStore8: 1, IntrinsicStore16: 2,
	IntrinsicStore32: 4, IntrinsicStore64: 8,
}

// ---------------------------------------------------------------------------------------------------------------------
