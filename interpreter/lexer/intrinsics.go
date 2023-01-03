package lexer

import "Gorth/interpreter/utils"

type IntrinsicType int

const (
	IntrinsicPlus IntrinsicType = iota
	IntrinsicMinus
	IntrinsicMul
	IntrinsicDiv
	IntrinsicMod

	IntrinsicShl
	IntrinsicShr
	IntrinsicBitAnd
	IntrinsicBitOr
	IntrinsicBitXor

	IntrinsicLogicalAnd
	IntrinsicLogicalOr
	IntrinsicLogicalNot

	IntrinsicEq
	IntrinsicNe
	IntrinsicLe
	IntrinsicGe
	IntrinsicLt
	IntrinsicGt

	IntrinsicDup
	IntrinsicSwap
	IntrinsicDrop
	IntrinsicOver
	IntrinsicRot

	IntrinsicPuti
	IntrinsicPuts
	IntrinsicPutc

	IntrinsicDebug

	IntrinsicLoad8
	IntrinsicStore8
	IntrinsicLoad16
	IntrinsicStore16
	IntrinsicLoad32
	IntrinsicStore32
	IntrinsicLoad64
	IntrinsicStore64

	IntrinsicArgc
	IntrinsicArgv

	IntrinsicSyscall

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

	// TODO: BitNot(X) =  ^X

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
	IntrinsicArgv: "argv",

	IntrinsicSyscall: "syscall",
}

var WordToIntrinsic = utils.RevMap(IntrinsicName).(map[string]IntrinsicType)
