package lexer

// Type for different tokens
type TokenType int

// Gorth tokens constants
const (
	TokenInt TokenType = iota
	TokenBool
	TokenWord
	TokenKeyword
	TokenString
	TokenChar

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

var TokenTypeName = map[TokenType]string{
	TokenInt: "TokenInt",
	TokenBool: "TokenBool",
	TokenWord: "TokenWord",
	TokenKeyword: "TokenKeyword",
	TokenString: "TokenString",
	TokenChar: "TokenChar",
}
