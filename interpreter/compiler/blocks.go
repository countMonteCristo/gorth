package compiler

import (
	"Gorth/interpreter/keywords"
	"Gorth/interpreter/tokens"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
)

// ---------------------------------------------------------------------------------------------------------------------

type BlockStack = utils.Stack[*Block]

// ---------------------------------------------------------------------------------------------------------------------

// Jumps are: break and continue
type Jump struct {
	Keyword keywords.KeywordType
	Addr    types.IntType
}

// ---------------------------------------------------------------------------------------------------------------------

// Blocks are: if-else-end, while-do-end, func-do-end
type Block struct {
	Addr   types.IntType // absolute block start address
	Tok    *tokens.Token
	Jumps  []Jump
	Typ    keywords.KeywordType
	Parent *Block
	Data   interface{}
}

func NewBlock(addr types.IntType, token *tokens.Token, typ keywords.KeywordType, data interface{}, parent *Block) *Block {
	return &Block{Addr: addr, Tok: token, Jumps: make([]Jump, 0), Typ: typ, Data: data, Parent: parent}
}

// ---------------------------------------------------------------------------------------------------------------------
