package lexer

type Jump struct {
	Keyword KeywordType
	Addr    int
}

type Block struct {
	Addr  int
	Tok   Token
	Jumps []Jump
}
