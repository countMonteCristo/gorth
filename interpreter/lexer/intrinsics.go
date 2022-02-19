package lexer

import "GoStudy/Gorth/interpreter/utils"

type IntrinsicType int

const (
	IntrinsicPlus  IntrinsicType = iota
	IntrinsicMinus IntrinsicType = iota
	IntrinsicMul   IntrinsicType = iota
	IntrinsicDiv   IntrinsicType = iota
	IntrinsicMod   IntrinsicType = iota

	IntrinsicShl    IntrinsicType = iota
	IntrinsicShr    IntrinsicType = iota
	IntrinsicBitAnd IntrinsicType = iota
	IntrinsicBitOr  IntrinsicType = iota
	IntrinsicBitXor IntrinsicType = iota

	IntrinsicLogicalAnd IntrinsicType = iota
	IntrinsicLogicalOr  IntrinsicType = iota
	IntrinsicLogicalNot IntrinsicType = iota

	IntrinsicEq IntrinsicType = iota
	IntrinsicNe IntrinsicType = iota
	IntrinsicLe IntrinsicType = iota
	IntrinsicGe IntrinsicType = iota
	IntrinsicLt IntrinsicType = iota
	IntrinsicGt IntrinsicType = iota

	IntrinsicDup  IntrinsicType = iota
	IntrinsicSwap IntrinsicType = iota
	IntrinsicDrop IntrinsicType = iota
	IntrinsicOver IntrinsicType = iota
	IntrinsicRot  IntrinsicType = iota

	IntrinsicPuti IntrinsicType = iota
	IntrinsicPuts IntrinsicType = iota
	IntrinsicPutc IntrinsicType = iota

	IntrinsicDebug IntrinsicType = iota

	IntrinsicLoad8   IntrinsicType = iota
	IntrinsicStore8  IntrinsicType = iota
	IntrinsicLoad16  IntrinsicType = iota
	IntrinsicStore16 IntrinsicType = iota
	IntrinsicLoad32  IntrinsicType = iota
	IntrinsicStore32 IntrinsicType = iota
	IntrinsicLoad64  IntrinsicType = iota
	IntrinsicStore64 IntrinsicType = iota

	IntrinsicArgc IntrinsicType = iota

	IntrinsicCount = iota
)

var IntrinsicName = map[IntrinsicType]string{
	IntrinsicPlus:  "+",
	IntrinsicMinus: "-",
	IntrinsicMul:   "*",
	IntrinsicDiv:   "/",
	IntrinsicMod:   "%",

	IntrinsicShl:    "<<",
	IntrinsicShr:    ">>",
	IntrinsicBitAnd: "&",
	IntrinsicBitOr:  "|",
	IntrinsicBitXor: "^",

	IntrinsicEq: "=",
	IntrinsicNe: "!=",
	IntrinsicLe: "<=",
	IntrinsicGe: ">=",
	IntrinsicLt: "<",
	IntrinsicGt: ">",

	IntrinsicLogicalAnd: "&&",
	IntrinsicLogicalOr:  "||",
	IntrinsicLogicalNot: "!",

	IntrinsicDup:  "dup",
	IntrinsicSwap: "swap",
	IntrinsicDrop: "drop",
	IntrinsicOver: "over",
	IntrinsicRot:  "rot",

	IntrinsicPuti: "puti",
	IntrinsicPuts: "puts",
	IntrinsicPutc: "putc",

	IntrinsicDebug: "???",

	IntrinsicLoad8:   "@8",
	IntrinsicStore8:  "!8",
	IntrinsicLoad16:  "@16",
	IntrinsicStore16: "!16",
	IntrinsicLoad32:  "@32",
	IntrinsicStore32: "!32",
	IntrinsicLoad64:  "@64",
	IntrinsicStore64: "!64",

	IntrinsicArgc: "argc",
}

var WordToIntrinsic = utils.RevMap(IntrinsicName).(map[string]IntrinsicType)
