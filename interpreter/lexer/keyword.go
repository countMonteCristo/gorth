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

	KeywordConst
	KeywordAlloc

	KeywordFunc

	KeywordInclude

	KeywordCount = iota
)

var KeywordName = map[KeywordType]string{
	KeywordIf:   "if",
	KeywordElse: "else",
	KeywordEnd:  "end",

	KeywordWhile:    "while",
	KeywordDo:       "do",
	KeywordBreak:    "break",
	KeywordContinue: "continue",

	KeywordConst: "const",
	KeywordAlloc: "alloc",

	KeywordFunc:   "func",

	KeywordInclude: "include",
}

var WordToKeyword = utils.RevMap(KeywordName).(map[string]KeywordType)
