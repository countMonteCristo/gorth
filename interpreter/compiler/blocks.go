package compiler

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
)

// ---------------------------------------------------------------------------------------------------------------------

type BlockStack = utils.Stack[*Block]

// ---------------------------------------------------------------------------------------------------------------------

// Jumps are: break and continue
type Jump struct {
	Keyword lexer.KeywordType
	Addr    types.IntType
}

// ---------------------------------------------------------------------------------------------------------------------

// Blocks are: if-else-end, while-do-end, func-do-end
type Block struct {
	Addr   types.IntType // absolute block start address
	Tok    lexer.Token
	Jumps  []Jump
	Typ    lexer.KeywordType
	Parent *Block
	Data   interface{}
}

func NewBlock(addr types.IntType, token *lexer.Token, typ lexer.KeywordType, data interface{}, parent *Block) *Block {
	return &Block{Addr: addr, Tok: *token, Jumps: make([]Jump, 0), Typ: typ, Data: data, Parent: parent}
}

// ---------------------------------------------------------------------------------------------------------------------
