package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
)

type Binary[T any] func(x, y T) T

var SafeArithmeticFunctions = map[lexer.IntrinsicType]Binary[types.IntType]{
	lexer.IntrinsicPlus:   func(x, y types.IntType) types.IntType { return x + y },
	lexer.IntrinsicMinus:  func(x, y types.IntType) types.IntType { return x - y },
	lexer.IntrinsicMul:    func(x, y types.IntType) types.IntType { return x * y },
	lexer.IntrinsicBitAnd: func(x, y types.IntType) types.IntType { return x & y },
	lexer.IntrinsicBitOr:  func(x, y types.IntType) types.IntType { return x | y },
	lexer.IntrinsicBitXor: func(x, y types.IntType) types.IntType { return x ^ y },
}

var LogicalFunctions = map[lexer.IntrinsicType]Binary[types.BoolType]{
	lexer.IntrinsicLogicalAnd: func(x, y types.BoolType) types.BoolType { return x && y },
	lexer.IntrinsicLogicalOr:  func(x, y types.BoolType) types.BoolType { return x || y },
}

var ComparableFunctions = map[lexer.IntrinsicType]func(x, y types.IntType) types.BoolType{
	lexer.IntrinsicEq: func(x, y types.IntType) types.BoolType { return x == y },
	lexer.IntrinsicNe: func(x, y types.IntType) types.BoolType { return x != y },
	lexer.IntrinsicLe: func(x, y types.IntType) types.BoolType { return x <= y },
	lexer.IntrinsicGe: func(x, y types.IntType) types.BoolType { return x >= y },
	lexer.IntrinsicLt: func(x, y types.IntType) types.BoolType { return x < y },
	lexer.IntrinsicGt: func(x, y types.IntType) types.BoolType { return x > y },
}

var LoadSizes = map[lexer.IntrinsicType]int{
	lexer.IntrinsicLoad8: 1, lexer.IntrinsicLoad16: 2,
	lexer.IntrinsicLoad32: 4, lexer.IntrinsicLoad64: 8,
}

var StoreSizes = map[lexer.IntrinsicType]int{
	lexer.IntrinsicStore8: 1, lexer.IntrinsicStore16: 2,
	lexer.IntrinsicStore32: 4, lexer.IntrinsicStore64: 8,
}
