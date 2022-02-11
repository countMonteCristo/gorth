package lexer

type Jump struct {
	Text string
	Addr int
}

type Block struct {
	Addr int
	Tok  Token
	Jumps []Jump
}
