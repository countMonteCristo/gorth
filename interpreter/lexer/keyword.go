package lexer

import "Gorth/interpreter/utils"

type KeywordType int

const (
	KeywordIf KeywordType = iota
	KeywordElse
	KeywordEnd

	KeywordWhile
	KeywordDo
	KeywordBreak
	KeywordContinue
	KeywordReturn

	KeywordConst
	KeywordAlloc

	KeywordFunc
	KeywordColon

	KeywordInclude
)

var KeywordName = map[KeywordType]string{
	KeywordIf:   "if",
	KeywordElse: "else",
	KeywordEnd:  "end",

	KeywordWhile:    "while",
	KeywordDo:       "do",
	KeywordBreak:    "break",
	KeywordContinue: "continue",
	KeywordReturn:   "return",

	KeywordConst: "const",
	KeywordAlloc: "alloc",

	KeywordFunc:  "func",
	KeywordColon: ":",

	KeywordInclude: "include",
}

var WordToKeyword = utils.RevMap(KeywordName).(map[string]KeywordType)
