package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
)

// Jumps are: break and continue
type Jump struct {
	Keyword lexer.KeywordType
	Addr    types.IntType
}

// Blocks are: if-else-end, while-do-end, func-do-end
type Block struct {
	Addr  types.IntType // absolute block start address
	Tok   lexer.Token
	Jumps []Jump
}

func NewBlock(addr types.IntType, token *lexer.Token) *Block {
	return &Block{Addr: addr, Tok: *token, Jumps: make([]Jump, 0)}
}

type Function struct {
	Name    string
	Addr    types.IntType
}
