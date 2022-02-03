package lexer

// Type for different tokens
type TokenType int

// Gorth tokens constants
const (
	TokenInt     TokenType = iota
	TokenBool    TokenType = iota
	TokenWord    TokenType = iota
	TokenKeyword TokenType = iota
	TokenString  TokenType = iota
	TokenChar    TokenType = iota

	TokenCount = iota
)

type Location struct {
	Filepath string
	Line     int
	Column   int
}

type Token struct {
	Typ   TokenType
	Text  string
	Value interface{}
	Loc   Location
}
