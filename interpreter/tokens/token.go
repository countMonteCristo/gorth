package tokens

import "Gorth/interpreter/utils"

// ---------------------------------------------------------------------------------------------------------------------

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
)

// ---------------------------------------------------------------------------------------------------------------------

type Token struct {
	Typ   TokenType
	Text  string
	Value interface{}
	Loc   utils.Location
}

// ---------------------------------------------------------------------------------------------------------------------

var TokenType2Str = map[TokenType]string{
	TokenInt:     "TokenInt",
	TokenBool:    "TokenBool",
	TokenWord:    "TokenWord",
	TokenKeyword: "TokenKeyword",
	TokenString:  "TokenString",
	TokenChar:    "TokenChar",
}

// ---------------------------------------------------------------------------------------------------------------------
