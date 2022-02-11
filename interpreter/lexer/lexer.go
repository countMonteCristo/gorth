package lexer

import (
	"GoStudy/Gorth/interpreter/types"
	"GoStudy/Gorth/interpreter/utils"
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
			CompilerFatal(&lx.Loc, fmt.Sprintf("Unknown escape character: %s", data[pos:pos+2]))
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
			CompilerFatal(&lx.Loc, fmt.Sprintf("Expecting to find closing \" for string literal, but got <%s>", string(chars[len(chars)-1])))
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
			CompilerFatal(&lx.Loc, fmt.Sprintf("Expecting to find closing ' for char literal, but got <%s>", string(data[pos])))
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

		// fmt.Printf("Chopped word: <%s> empty=%t\n", word, empty)

		lx.Col = lx.Loc.Column + len(word) + 1

		if empty {
			lx.Row++
			lx.Col = 0
			continue
		}

		token = Token{Text: word, Loc: lx.Loc}

		// check if word is string literal
		if word[0] == '"' {
			// fmt.Printf("<%s> - is a string\n", word)
			token.Typ = TokenString
			token.Value = word[1 : len(word)-1]
			return
		}

		// check if word is char literal
		if word[0] == '\'' {
			// fmt.Printf("<%s> - is a char\n", word)
			token.Typ = TokenChar
			token.Value = int(word[1])
			return
		}

		// check if word is int literal
		number, err := strconv.Atoi(word)
		if err == nil {
			// fmt.Printf("<%s> - is an int\n", word)
			token.Typ = TokenInt
			token.Value = number
			return
		}

		// check if word is boolean literal
		boolean, exists := types.WordToBool[word]
		if exists {
			// fmt.Printf("<%s> - is a boolean\n", word)
			token.Typ = TokenBool
			token.Value = boolean
			return
		}

		// check if word is keyword
		// fmt.Printf("try to create token as keyword\n")
		keyword, exists := WordToKeyword[word]
		if exists {
			// fmt.Printf("<%s> - is a keyword\n", word)
			token.Typ = TokenKeyword
			token.Value = keyword
			return
		}

		// word is some name
		// fmt.Printf("<%s> - is a word\n", word)
		token.Typ = TokenWord
		token.Value = word
		return
	}

	end = true
	return
}

func (lx *Lexer) const_eval(name_token *Token, tokens *[]Token, consts *map[string]int) (value int) {
	const_stack := &utils.Stack{}
	for _, token := range *tokens {
		switch token.Typ {
		case TokenInt:
			const_stack.Push(token.Value.(int))
		case TokenWord:
			intrinsic, exists := WordToIntrinsic[token.Text]
			if exists {
				switch intrinsic {
				case IntrinsicPlus:
					b := const_stack.Pop().(int)
					a := const_stack.Pop().(int)
					const_stack.Push(a + b)
				case IntrinsicMinus:
					b := const_stack.Pop().(int)
					a := const_stack.Pop().(int)
					const_stack.Push(a - b)
				case IntrinsicMul:
					b := const_stack.Pop().(int)
					a := const_stack.Pop().(int)
					const_stack.Push(a * b)
				case IntrinsicDiv:
					b := const_stack.Pop().(int)
					if b == 0 {
						CompilerFatal(&token.Loc, "Division by zero")
						utils.Exit(1)
					}
					a := const_stack.Pop().(int)
					const_stack.Push(a / b)
				case IntrinsicMod:
					b := const_stack.Pop().(int)
					if b == 0 {
						CompilerFatal(&token.Loc, "Division by zero")
						utils.Exit(1)
					}
					a := const_stack.Pop().(int)
					const_stack.Push(a % b)
				case IntrinsicShl:
					b := const_stack.Pop().(int)
					if b < 0 {
						CompilerFatal(&token.Loc, fmt.Sprintf("Negative shift amount in `<<`: %d", b))
						utils.Exit(1)
					}
					a := const_stack.Pop().(int)
					const_stack.Push(a << b)
				case IntrinsicShr:
					b := const_stack.Pop().(int)
					if b < 0 {
						CompilerFatal(&token.Loc, fmt.Sprintf("Negative shift amount in `>>`: %d", b))
						utils.Exit(1)
					}
					a := const_stack.Pop().(int)
					const_stack.Push(a >> b)

				default:
					CompilerFatal(
						&token.Loc,
						fmt.Sprintf(
							"Unexpected intrinsic in const-block compile-time "+
								"evaluation: %s. Supported: [+, -, *, /, %%, >>, <<]",
							token.Text,
						),
					)
					utils.Exit(1)
				}
				continue
			}

			val, exists := (*consts)[token.Text]
			if exists {
				const_stack.Push(val)
				continue
			}

			CompilerFatal(&token.Loc, fmt.Sprintf("Unsupported word in compile-time const-block evaluation: %s", token.Text))
			utils.Exit(1)
		default:
			CompilerFatal(&token.Loc, fmt.Sprintf("Unsupported token in compile-time const-block evaluation: %s", token.Text))
			utils.Exit(1)
		}
	}

	if const_stack.Size() > 1 {
		CompilerFatal(&name_token.Loc, "Unhandled data in compile-time const-block evaluation stack")
		utils.Exit(1)
	}

	value = const_stack.Pop().(int)
	return
}

func (lx *Lexer) parse_const_block(token *Token, typ string, names *map[string]Token, consts *map[string]int) (tok Token, const_value int) {
	tok, end := lx.next_token()
	if end {
		CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name, but got nothing", typ))
		utils.Exit(1)
	}
	if tok.Typ != TokenWord {
		CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name to be a word, but got %s", typ, tok.Text))
		utils.Exit(1)
	}
	defined_token, exists := (*names)[tok.Text]
	if exists {
		CompilerFatal(&tok.Loc, fmt.Sprintf("Redefinition of word <%s>", tok.Text))
		CompilerInfo(&defined_token.Loc, "Previously defined here")
		utils.Exit(1)
	}

	const_block := make([]Token, 0)
	for {
		tok, end := lx.next_token()
		if end {
			CompilerFatal(&token.Loc, fmt.Sprintf("Unexpected end while processing `%s` block", typ))
			utils.Exit(1)
		}

		if tok.Typ == TokenKeyword && tok.Value.(KeywordType) == KeywordEnd {
			break
		}

		const_block = append(const_block, tok)
	}

	const_value = lx.const_eval(&tok, &const_block, consts)
	return
}

func (lx *Lexer) ProcessFile(fn string, ctx *Context) (tokens []Token) {
	lx.Fn = fn
	lx.Row = 0
	lx.Col = 0

	lx.Lines = utils.ReadFile(lx.Fn)
	for {
		token, end := lx.next_token()
		if end {
			break
		}
		// fmt.Printf("Got token: <%s>\n", token.Text)
		switch token.Typ {
		case TokenInt:
			tokens = append(tokens, token)
		case TokenBool:
			tokens = append(tokens, token)
		case TokenString:
			tokens = append(tokens, token)
		case TokenChar:
			tokens = append(tokens, token)
		case TokenKeyword:
			switch token.Value.(KeywordType) {
			case KeywordIf:
				tokens = append(tokens, token)
			case KeywordElse:
				tokens = append(tokens, token)
			case KeywordEnd:
				tokens = append(tokens, token)
			case KeywordDo:
				tokens = append(tokens, token)
			case KeywordWhile:
				tokens = append(tokens, token)
			case KeywordBreak:
				tokens = append(tokens, token)
			case KeywordConst:
				tok, const_value := lx.parse_const_block(&token, token.Text, &ctx.Names, &ctx.Consts)

				(*ctx).Consts[tok.Text] = const_value
				(*ctx).Names[tok.Text] = tok
			case KeywordAlloc:
				tok, alloc_size := lx.parse_const_block(&token, token.Text, &ctx.Names, &ctx.Consts)
				if alloc_size < 0 {
					CompilerFatal(&tok.Loc, fmt.Sprintf("Negative size for `alloc` block: %d", alloc_size))
					utils.Exit(1)
				}

				(*ctx).Allocs[tok.Text] = (*ctx).Memory.MemPtr
				(*ctx).Memory.MemPtr += alloc_size
				(*ctx).Names[tok.Text] = tok
			default:
				CompilerFatal(&token.Loc, fmt.Sprintf("Unhandled keyword `%s` in lexer.ProcessFile", token.Text))
				utils.Exit(1)
			}

		case TokenWord:
			intrinsic, exists := WordToIntrinsic[token.Text]
			if exists {
				token.Value = intrinsic
				tokens = append(tokens, token)
				continue
			}

			ptr, exists := (*ctx).Allocs[token.Text]
			if exists {
				token.Typ = TokenInt
				token.Value = ptr
				tokens = append(tokens, token)
				continue
			}

			val, exists := (*ctx).Consts[token.Text]
			if exists {
				token.Typ = TokenInt
				token.Value = val
				tokens = append(tokens, token)
				continue
			}

			CompilerFatal(&token.Loc, fmt.Sprintf("Unknown word: %s", token.Text))
			utils.Exit(1)
		default:
			CompilerFatal(&token.Loc, "Unhandled Token.Typ in Lexer.process_file")
			utils.Exit(1)
		}

	}
	return
}
