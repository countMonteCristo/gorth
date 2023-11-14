package keywords

import "Gorth/interpreter/utils"

// ---------------------------------------------------------------------------------------------------------------------

type KeywordType int

const (
	KeywordIf KeywordType = iota
	KeywordElif
	KeywordElse
	KeywordEnd

	KeywordWhile
	KeywordDo
	KeywordBreak
	KeywordContinue
	KeywordReturn

	KeywordConst
	KeywordAlloc
	KeywordCapture

	KeywordInline
	KeywordFunc
	KeywordColon

	KeywordInclude

	KeywordFptrOf
	KeywordCallLike
)

// ---------------------------------------------------------------------------------------------------------------------

var Keyword2Str = map[KeywordType]string{
	KeywordIf:   "if",
	KeywordElif: "elif",
	KeywordElse: "else",
	KeywordEnd:  "end",

	KeywordWhile:    "while",
	KeywordDo:       "do",
	KeywordBreak:    "break",
	KeywordContinue: "continue",
	KeywordReturn:   "return",

	KeywordConst:   "const",
	KeywordAlloc:   "alloc",
	KeywordCapture: "capture",

	KeywordInline: "inline",
	KeywordFunc:   "func",
	KeywordColon:  ":",

	KeywordInclude: "include",

	KeywordFptrOf:   "fptr-of",
	KeywordCallLike: "call-like",
}

var Str2Keyword = utils.RevMap(Keyword2Str).(map[string]KeywordType)

// ---------------------------------------------------------------------------------------------------------------------
