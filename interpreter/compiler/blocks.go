package compiler

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"
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
	Data  interface{}
}

func NewBlock(addr types.IntType, token *lexer.Token, typ lexer.KeywordType, data interface{}) *Block {
	return &Block{Addr: addr, Tok: *token, Jumps: make([]Jump, 0), Typ: typ, Data: data}
}

type FuncSignature struct {
	Name    string
	Inputs  utils.Stack
	Outputs utils.Stack
}

type Function struct {
	Sig     FuncSignature
	Addr    types.IntType
	Inlined bool
	Ops     []vm.Op
}
