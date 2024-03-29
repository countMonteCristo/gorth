package intrinsics

import "Gorth/interpreter/utils"

// ---------------------------------------------------------------------------------------------------------------------

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
	IntrinsicBitNot

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

	IntrinsicDebug
	IntrinsicTypeDebug

	IntrinsicAssert

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
	IntrinsicEnv

	IntrinsicOffset
	IntrinsicReset

	IntrinsicSyscall0
	IntrinsicSyscall1
	IntrinsicSyscall2
	IntrinsicSyscall3
	IntrinsicSyscall4
	IntrinsicSyscall5
	IntrinsicSyscall6

	IntrinsicCastInt
	IntrinsicCastPtr
	IntrinsicCastBool
	IntrinsicCastFptr
)

// ---------------------------------------------------------------------------------------------------------------------

var Intrinsic2Str = map[IntrinsicType]string{
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
	IntrinsicBitNot: "~",

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

	IntrinsicDebug:     "???",
	IntrinsicTypeDebug: "!!!",

	IntrinsicAssert: "assert",

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
	IntrinsicEnv:  "env",

	IntrinsicOffset: "offset",
	IntrinsicReset:  "reset",

	IntrinsicSyscall0: "syscall0",
	IntrinsicSyscall1: "syscall1",
	IntrinsicSyscall2: "syscall2",
	IntrinsicSyscall3: "syscall3",
	IntrinsicSyscall4: "syscall4",
	IntrinsicSyscall5: "syscall5",
	IntrinsicSyscall6: "syscall6",

	IntrinsicCastInt:  "cast(int)",
	IntrinsicCastPtr:  "cast(ptr)",
	IntrinsicCastBool: "cast(bool)",
	IntrinsicCastFptr: "cast(fptr)",
}

var Str2Intrinsic = utils.RevMap(Intrinsic2Str).(map[string]IntrinsicType)

// ---------------------------------------------------------------------------------------------------------------------
