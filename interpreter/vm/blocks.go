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
	Typ   lexer.KeywordType
}

func NewBlock(addr types.IntType, token *lexer.Token, typ lexer.KeywordType) *Block {
	return &Block{Addr: addr, Tok: *token, Jumps: make([]Jump, 0), Typ: typ}
}

type FuncSignature struct {
	Name    string
	Inputs  []lexer.DataType
	Outputs []lexer.DataType
}

type Function struct {
	Sig  FuncSignature
	Addr types.IntType
}
