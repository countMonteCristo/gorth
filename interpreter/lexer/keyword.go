package lexer

import "GoStudy/Gorth/interpreter/utils"

type KeywordType int

const (
	KeywordIf   KeywordType = iota
	KeywordElse KeywordType = iota
	KeywordEnd  KeywordType = iota

	KeywordWhile    KeywordType = iota
	KeywordDo       KeywordType = iota
	KeywordBreak    KeywordType = iota
	KeywordContinue KeywordType = iota

	KeywordConst KeywordType = iota
	KeywordAlloc KeywordType = iota

	KeywordFunc KeywordType = iota

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

	KeywordFunc: "func",
}

var WordToKeyword = utils.RevMap(KeywordName).(map[string]KeywordType)
