package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
)

type Jump struct {
	Keyword lexer.KeywordType
	Addr    types.IntType
}

type Block struct {
	Addr  types.IntType
	Tok   lexer.Token
	Jumps []Jump
}
