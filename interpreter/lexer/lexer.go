package lexer

import (
	"Gorth/interpreter/datatypes"
	"Gorth/interpreter/keywords"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/settings"
	"Gorth/interpreter/tokens"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"

	"strconv"
	"strings"
)

// ---------------------------------------------------------------------------------------------------------------------

type Lexer struct {
	Fn    string
	Loc   utils.Location
	Lines []string
	Row   int
	Col   int
}

// ---------------------------------------------------------------------------------------------------------------------

func (lx *Lexer) ChopChar(data string, pos int) (b byte, escaped bool, err error) {
	escaped = false
	if data[pos] == '\\' {
		escaped = true
		if pos+1 == len(data) {
			err = logger.LexerError(&lx.Loc, "Unexpected end of escaped char literal")
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
			err = logger.LexerError(&lx.Loc, "Unknown escape character: `%s`", data[pos:pos+2])
			return
		}
	} else {
		b = data[pos]
	}
	return
}

func (lx *Lexer) ChopWord(data string, line int, pos int) (word string, empty bool, npos int, err error) {
	npos = pos
	empty = false
	for npos < len(data) {
		if strings.IndexByte(" \t", data[npos]) != -1 {
			npos++
			continue
		} else {
			break
		}
	}

	lx.Loc = utils.Location{Filepath: lx.Fn, Line: line, Column: npos}
	if npos == len(data) {
		empty = true
		return
	}

	start := npos
	switch data[npos] {
	case '"':
		npos++
		chars := make([]byte, 0)
		chars = append(chars, '"')
		closed := false
		for npos < len(data) {
			b, escaped, terr := lx.ChopChar(data, npos)
			if terr != nil {
				err = terr
				return
			}
			npos++
			if escaped {
				npos++
			}
			chars = append(chars, b)
			if b == '"' && !escaped {
				closed = true
				break
			}
		}
		if !closed {
			err = logger.LexerError(&lx.Loc, "Expecting to find closing \" for string literal, but got `%s`", string(chars[len(chars)-1]))
			if err != nil {
				return
			}
		}
		word = string(chars)
	case '\'':
		npos++
		b, escaped, terr := lx.ChopChar(data, npos)
		if terr != nil {
			err = terr
			return
		}
		npos++
		if escaped {
			npos++
		}
		if npos == len(data) {
			err = logger.LexerError(&lx.Loc, "Unexpecting end of char literal")
			return
		}
		if data[npos] != '\'' {
			err = logger.LexerError(&lx.Loc, "Expecting to find closing ' for char literal, but got `%s`", string(data[npos]))
			return
		}
		word = "'" + string(b) + "'"
		npos++
	default:
		for npos < len(data) && data[npos] != ' ' {
			if data[npos] == ' ' {
				break
			}
			if npos < len(data)-1 && data[npos:npos+2] == "//" {
				empty = true
				break
			}
			npos++
		}
		word = data[start:npos]
	}

	return
}

// ---------------------------------------------------------------------------------------------------------------------

func (lx *Lexer) next_token() (token tokens.Token, end bool, err error) {
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
		word, empty, pos, terr := lx.ChopWord(lx.Lines[lx.Row], lx.Row, lx.Col)
		if terr != nil {
			err = terr
			return
		}

		// fmt.Printf("Chopped word: `%s` empty=%t\n", word, empty)

		lx.Col = pos + 1

		if empty {
			lx.Row++
			lx.Col = 0
			continue
		}

		token = tokens.Token{Text: word, Loc: lx.Loc}

		// check if word is string literal
		if word[0] == '"' {
			// fmt.Printf("`%s` - is a string\n", word)
			token.Typ = tokens.TokenString
			token.Value = word[1 : len(word)-1]
			return
		}

		// check if word is char literal
		if word[0] == '\'' {
			// fmt.Printf("`%s` - is a char\n", word)
			token.Typ = tokens.TokenChar
			token.Value = types.IntType(word[1])
			return
		}

		// check if word is boolean literal
		boolean, exists := types.Str2Bool[word]
		if exists {
			// fmt.Printf("`%s` - is a boolean\n", word)
			token.Typ = tokens.TokenBool
			token.Value = boolean
			return
		}

		// check if word is keyword
		// fmt.Printf("try to create token as keyword\n")
		keyword, exists := keywords.Str2Keyword[word]
		if exists {
			// fmt.Printf("`%s` - is a keyword\n", word)
			token.Typ = tokens.TokenKeyword
			token.Value = keyword
			return
		}

		// check if word is int literal
		number, terr := strconv.ParseInt(word, 10, 64)
		if terr == nil {
			// fmt.Printf("`%s` - is an int\n", word)
			token.Typ = tokens.TokenInt
			token.Value = number
			return
		}

		if len(word) > 2 && word[0] == '0' {
			w := word[2:]
			base := -1
			switch word[1] {
			case 'x':
				base = 16
			case 'o':
				base = 8
			case 'b':
				base = 2
			}
			if number, terr = strconv.ParseInt(w, base, 64); terr == nil {
				token.Typ = tokens.TokenInt
				token.Value = number
				return
			}
		}

		datatype, exists := datatypes.Str2DataType[word]
		if exists {
			// fmt.Printf("`%s` - is a type\n", word)
			token.Typ = tokens.TokenWord
			token.Value = datatype
			return
		}

		// word is some name
		// fmt.Printf("`%s` - is a word\n", word)
		token.Typ = tokens.TokenWord
		token.Value = word
		return
	}

	end = true
	return
}

// ---------------------------------------------------------------------------------------------------------------------

func (lx *Lexer) ProcessFile(fn string, import_path []string, imp *Importer, s *settings.Settings) (th *tokens.TokenHolder, err error) {
	defer logger.Timeit(logger.ModuleLexer, s.LogLevel)()

	lx.Fn = fn
	lx.Row = 0
	lx.Col = 0

	lx.Lines = utils.ReadFile(lx.Fn)

	th = tokens.NewTokenHolder()

	for {
		token, end, terr := lx.next_token()
		if terr != nil {
			err = terr
			return
		}

		if end {
			break
		}

		if token.Typ == tokens.TokenKeyword {
			kw_type := token.Value.(keywords.KeywordType)
			if kw_type == keywords.KeywordInclude {
				next, end, terr := lx.next_token()
				if terr != nil {
					err = terr
					return
				}

				if end {
					err = logger.LexerError(&token.Loc, "Expected import file path to be a string, but got nothing")
					return
				}
				if next.Typ != tokens.TokenString {
					err = logger.LexerError(
						&token.Loc, "Expected import file path to be a %s, but got %s",
						tokens.TokenType2Str[tokens.TokenString], tokens.TokenType2Str[next.Typ],
					)
					return
				}

				// TODO: do proper checks if files have been already imported
				imported_fn := next.Value.(string)
				full_imported_fn, exists := imp.Find(fn, imported_fn)
				if !exists {
					err = logger.LexerError(&next.Loc, "Can not import file %s: not in Paths", full_imported_fn)
					return
				}

				for _, prev_fn := range import_path {
					if prev_fn == full_imported_fn {
						err = logger.LexerError(&next.Loc, "Circular imports detected when trying to include %s", imported_fn)
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
				included_tokens, ierr := new_lexer.ProcessFile(full_imported_fn, new_path, imp, s)
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

// ---------------------------------------------------------------------------------------------------------------------
