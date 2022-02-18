package vm

import "GoStudy/Gorth/interpreter/lexer"

type Jump struct {
	Keyword lexer.KeywordType
	Addr    int
}

type Block struct {
	Addr  int
	Tok   lexer.Token
	Jumps []Jump
}
