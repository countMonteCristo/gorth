package lexer

import (
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"

	"strconv"
	"strings"
)

type Lexer struct {
	Fn    string
	Loc   utils.Location
	Lines []string
	Row   int
	Col   int
}

type TokenHolder struct {
	tokens []Token
	idx    int
}

func NewTokenHolder() *TokenHolder {
	return &TokenHolder{tokens: make([]Token, 0), idx: 0}
}

func (th *TokenHolder) AppendTokensFrom(oth *TokenHolder) {
	th.tokens = append(th.tokens, oth.tokens...)
}
func (th *TokenHolder) AppendToken(token Token) {
	th.tokens = append(th.tokens, token)
}

func (th *TokenHolder) GetNextToken() (token *Token) {
	token = &(th.tokens[th.idx])
	th.idx++
	return
}

func (th *TokenHolder) NextToken() *Token {
	return &(th.tokens[th.idx])
}

func (th *TokenHolder) Empty() bool {
	return th.idx >= len(th.tokens)
}

func (th *TokenHolder) Reset() {
	th.idx = 0
}

func (lx *Lexer) ChopChar(data string, pos int) (b byte, escaped bool, err error) {
	escaped = false
	if data[pos] == '\\' {
		escaped = true
		if pos+1 == len(data) {
			err = logger.FormatErrMsg(&lx.Loc, "Unexpected end of escaped char literal")
			return
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
			err = logger.FormatErrMsg(&lx.Loc, "Unknown escape character: `%s`", data[pos:pos+2])
			return
		}
	} else {
		b = data[pos]
	}
	return
}

func (lx *Lexer) ChopWord(data string, line int, pos int) (word string, empty bool, err error) {
	empty = false
	for pos < len(data) {
		if strings.IndexByte(" \t", data[pos]) != -1 {
			pos++
			continue
		} else {
			break
		}
	}

	lx.Loc = utils.Location{Filepath: lx.Fn, Line: line, Column: pos}
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
			b, escaped, terr := lx.ChopChar(data, pos)
			if terr != nil {
				err = terr
				return
			}
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
			err = logger.FormatErrMsg(&lx.Loc, "Expecting to find closing \" for string literal, but got `%s`", string(chars[len(chars)-1]))
			if err != nil {
				return
			}
		}
		word = string(chars)
	case '\'':
		pos++
		b, escaped, terr := lx.ChopChar(data, pos)
		if terr != nil {
			err = terr
			return
		}
		pos++
		if escaped {
			pos++
		}
		if pos == len(data) {
			err = logger.FormatErrMsg(&lx.Loc, "Unexpecting end of char literal")
			return
		}
		if data[pos] != '\'' {
			err = logger.FormatErrMsg(&lx.Loc, "Expecting to find closing ' for char literal, but got `%s`", string(data[pos]))
			return
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

func (lx *Lexer) next_token() (token Token, end bool, err error) {
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

	for lx.Row < len(lx.Lines) {
		// fmt.Printf("Try to chop word from line=%d col=%d\n", lx.Row+1, lx.Col+1)
		word, empty, terr := lx.ChopWord(lx.Lines[lx.Row], lx.Row, lx.Col)
		if terr != nil {
			err = terr
			return
		}

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
			token.Value = types.IntType(word[1])
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

		// check if word is int literal
		number, terr := strconv.ParseInt(word, 10, 64)
		if terr == nil {
			// fmt.Printf("`%s` - is an int\n", word)
			token.Typ = TokenInt
			token.Value = number
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

func (lx *Lexer) ProcessFile(fn string, import_path []string, imp *Importer) (th *TokenHolder, err error) {
	lx.Fn = fn
	lx.Row = 0
	lx.Col = 0

	lx.Lines = utils.ReadFile(lx.Fn)

	th = NewTokenHolder()

	for {
		token, end, terr := lx.next_token()
		if terr != nil {
			err = terr
			return
		}

		if end {
			break
		}

		if token.Typ == TokenKeyword {
			kw_type := token.Value.(KeywordType)
			if kw_type == KeywordInclude {
				next, end, terr := lx.next_token()
				if terr != nil {
					err = terr
					return
				}

				if end {
					err = logger.FormatErrMsg(&token.Loc, "Expected import file path to be a string, but got nothing")
					return
				}
				if next.Typ != TokenString {
					err = logger.FormatErrMsg(
						&token.Loc, "Expected import file path to be a %s, but got %s",
						TokenTypeName[TokenString], TokenTypeName[next.Typ],
					)
					return
				}

				imported_fn := next.Value.(string)
				full_imported_fn, exists := imp.Find(imported_fn)
				if !exists {
					err = logger.FormatErrMsg(&next.Loc, "Can not import file %s: not in Paths", full_imported_fn)
					return
				}

				for _, prev_fn := range import_path {
					if prev_fn == full_imported_fn {
						err = logger.FormatErrMsg(&next.Loc, "Circular imports detected when trying to include %s", imported_fn)
						return
					}
				}

				_, exists = imp.Included[full_imported_fn]
				if exists {
					// TODO: Log abot not importing file only in debug mode
					continue
				}

				new_path := append([]string{}, import_path...)
				new_path = append(new_path, fn)
				new_lexer := Lexer{}
				included_tokens, ierr := new_lexer.ProcessFile(full_imported_fn, new_path, imp)
				if ierr != nil {
					err = ierr
					return
				}

				th.AppendTokensFrom(included_tokens)
				continue
			}
		}

		th.AppendToken(token)
	}

	imp.Included[fn] = true

	return
}
