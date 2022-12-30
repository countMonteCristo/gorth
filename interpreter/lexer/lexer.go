package lexer

import (
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"

	"fmt"
	"strconv"
	"strings"
)

type Lexer struct {
	Fn    string
	Loc   Location
	Lines []string
	Row   int
	Col   int
}

func (lx *Lexer) ChopChar(data string, pos int) (b byte, escaped bool) {
	escaped = false
	if data[pos] == '\\' {
		escaped = true
		if pos+1 == len(data) {
			CompilerFatal(&lx.Loc, "Unexpected end of escaped char literal")
			utils.Exit(1)
		}
		switch data[pos+1] {
		case 'n':
			b = '\n'
		case 'r':
			b = '\r'
		case 't':
			b = '\t'
		case '"':
			b = '"'
		default:
			CompilerFatal(&lx.Loc, fmt.Sprintf("Unknown escape character: `%s`", data[pos:pos+2]))
			utils.Exit(1)
		}
	} else {
		b = data[pos]
	}
	return
}

func (lx *Lexer) ChopWord(data string, line int, pos int) (word string, empty bool) {
	empty = false
	for pos < len(data) {
		if strings.IndexByte(" \t", data[pos]) != -1 {
			pos++
			continue
		} else {
			break
		}
	}

	lx.Loc = Location{Filepath: lx.Fn, Line: line, Column: pos}
	if pos == len(data) {
		empty = true
		return
	}

	start := pos
	switch data[pos] {
	case '"':
		pos++
		chars := make([]byte, 0)
		chars = append(chars, '"')
		closed := false
		for pos < len(data) {
			b, escaped := lx.ChopChar(data, pos)
			pos++
			if escaped {
				pos++
			}
			chars = append(chars, b)
			if b == '"' && !escaped {
				closed = true
				break
			}
		}
		if !closed {
			CompilerFatal(&lx.Loc, fmt.Sprintf("Expecting to find closing \" for string literal, but got `%s`", string(chars[len(chars)-1])))
			utils.Exit(1)
		}
		word = string(chars)
	case '\'':
		pos++
		b, escaped := lx.ChopChar(data, pos)
		pos++
		if escaped {
			pos++
		}
		if pos == len(data) {
			CompilerFatal(&lx.Loc, "Unexpecting end of char literal")
			utils.Exit(1)
		}
		if data[pos] != '\'' {
			CompilerFatal(&lx.Loc, fmt.Sprintf("Expecting to find closing ' for char literal, but got `%s`", string(data[pos])))
			utils.Exit(1)
		}
		word = "'" + string(b) + "'"
		pos++
	default:
		for pos < len(data) && data[pos] != ' ' {
			if data[pos] == ' ' {
				break
			}
			if pos < len(data)-1 && data[pos:pos+2] == "//" {
				empty = true
				break
			}
			pos++
		}
		word = data[start:pos]
	}

	return
}

func (lx *Lexer) next_token() (token Token, end bool) {
	end = false
	if lx.Row >= len(lx.Lines) {
		end = true
		return
	}
	if lx.Col >= len(lx.Lines[lx.Row]) {
		lx.Row++
		lx.Col = 0
		if lx.Row == len(lx.Lines) {
			end = true
			return
		}
	}
	// assert(TokenCount == 6, "Unhandled Token in lex()")

	for lx.Row < len(lx.Lines) {
		// fmt.Printf("Try to chop word from line=%d col=%d\n", lx.Row+1, lx.Col+1)
		word, empty := lx.ChopWord(lx.Lines[lx.Row], lx.Row, lx.Col)

		// fmt.Printf("Chopped word: `%s` empty=%t\n", word, empty)

		lx.Col = lx.Loc.Column + len(word) + 1

		if empty {
			lx.Row++
			lx.Col = 0
			continue
		}

		token = Token{Text: word, Loc: lx.Loc}

		// check if word is string literal
		if word[0] == '"' {
			// fmt.Printf("`%s` - is a string\n", word)
			token.Typ = TokenString
			token.Value = word[1 : len(word)-1]
			return
		}

		// check if word is char literal
		if word[0] == '\'' {
			// fmt.Printf("`%s` - is a char\n", word)
			token.Typ = TokenChar
			token.Value = int(word[1])
			return
		}

		// check if word is int literal
		// TODO: parse as 64-bit integer
		number, err := strconv.Atoi(word)
		if err == nil {
			// fmt.Printf("`%s` - is an int\n", word)
			token.Typ = TokenInt
			token.Value = number
			return
		}

		// check if word is boolean literal
		boolean, exists := types.WordToBool[word]
		if exists {
			// fmt.Printf("`%s` - is a boolean\n", word)
			token.Typ = TokenBool
			token.Value = boolean
			return
		}

		// check if word is keyword
		// fmt.Printf("try to create token as keyword\n")
		keyword, exists := WordToKeyword[word]
		if exists {
			// fmt.Printf("`%s` - is a keyword\n", word)
			token.Typ = TokenKeyword
			token.Value = keyword
			return
		}

		// word is some name
		// fmt.Printf("`%s` - is a word\n", word)
		token.Typ = TokenWord
		token.Value = word
		return
	}

	end = true
	return
}

func (lx *Lexer) ProcessFile(fn string, import_path []string, imp *Importer) (tokens []Token) {
	lx.Fn = fn
	lx.Row = 0
	lx.Col = 0

	lx.Lines = utils.ReadFile(lx.Fn)

	for {
		token, end := lx.next_token()
		if end {
			break
		}

		if token.Typ == TokenKeyword {
			kw_type := token.Value.(KeywordType)
			if kw_type == KeywordInclude {
				next, end := lx.next_token()
				if end {
					CompilerFatal(&token.Loc, "Expected import file path as a string, got nothing")
					utils.Exit(1)
				}
				if next.Typ != TokenString {
					CompilerFatal(&token.Loc, fmt.Sprintf("Expected import file path as a string, got %s", next.Text))
					utils.Exit(1)
				}

				imported_fn := next.Value.(string)
				full_imported_fn, exists := imp.Find(imported_fn)
				if !exists {
					CompilerFatal(&next.Loc, fmt.Sprintf("Cannot import file %s: not in Paths", full_imported_fn))
					utils.Exit(1)
				}

				for _, prev_fn := range import_path {
					if prev_fn == full_imported_fn {
						CompilerFatal(&next.Loc, "Circular imports detected when trying to include "+imported_fn)
						utils.Exit(1)
					}
				}

				_, exists = imp.Included[full_imported_fn]
				if exists {
					// TODO: Log in info mode
					CompilerInfo(&next.Loc, fmt.Sprintf("File %s has been already imported, do nothing\n", fn))
					continue
				}

				new_path := append([]string{}, import_path...)
				new_path = append(new_path, fn)
				new_lexer := Lexer{}
				included_tokens := new_lexer.ProcessFile(full_imported_fn, new_path, imp)

				tokens = append(tokens, included_tokens...)
				continue
			}
		}

		tokens = append(tokens, token)
	}

	imp.Included[fn] = true

	return
}
