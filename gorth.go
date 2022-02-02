package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

func reverseMap(in map[string]interface{}) (result map[interface{}]interface{}) {
	for key, value := range in {
		result[value] = key
	}
	return
}

func assert(clause bool, msg string) {
	if !clause {
		log.Fatal(msg)
	}
}

type Location struct {
	Filepath string
	Line     int
	Column   int
}

type TokenType int

const (
	TokenInt     TokenType = iota
	TokenBool    TokenType = iota
	TokenWord    TokenType = iota
	TokenKeyword TokenType = iota
	TokenString  TokenType = iota
	TokenChar    TokenType = iota

	TokenCount = iota
)

const (
	StringLiteralBufferCap = 2 * 1024
)

var StringLiteralsBuffer = make([]string, 0, StringLiteralBufferCap)

type Token struct {
	Typ   TokenType
	Text  string
	Value interface{}
	Loc   Location
}

type KeywordType int

const (
	KeywordIf   KeywordType = iota
	KeywordElse KeywordType = iota
	KeywordEnd  KeywordType = iota

	KeywordWhile KeywordType = iota
	KeywordDo    KeywordType = iota

	KeywordConst KeywordType = iota
	KeywordAlloc KeywordType = iota

	KeywordCount = iota
)

var WordToKeyword = map[string]KeywordType{
	"if":   KeywordIf,
	"else": KeywordElse,
	"end":  KeywordEnd,

	"while": KeywordWhile,
	"do":    KeywordDo,

	"const": KeywordConst,
	"alloc": KeywordAlloc,
}
var KeywordName = map[KeywordType]string{
	KeywordIf:   "if",
	KeywordElse: "else",
	KeywordEnd:  "end",

	KeywordWhile: "while",
	KeywordDo:    "do",

	KeywordConst: "const",
	KeywordAlloc: "alloc",
}

type IntrinsicType int

const (
	IntrinsicPlus  IntrinsicType = iota
	IntrinsicMinus IntrinsicType = iota
	IntrinsicMul   IntrinsicType = iota
	IntrinsicDiv   IntrinsicType = iota
	IntrinsicMod   IntrinsicType = iota

	IntrinsicShl    IntrinsicType = iota
	IntrinsicShr    IntrinsicType = iota
	IntrinsicBitAnd IntrinsicType = iota
	IntrinsicBitOr  IntrinsicType = iota
	IntrinsicBitXor IntrinsicType = iota

	IntrinsicLogicalAnd IntrinsicType = iota
	IntrinsicLogicalOr  IntrinsicType = iota
	IntrinsicLogicalNot IntrinsicType = iota

	IntrinsicEq IntrinsicType = iota
	IntrinsicNe IntrinsicType = iota
	IntrinsicLe IntrinsicType = iota
	IntrinsicGe IntrinsicType = iota
	IntrinsicLt IntrinsicType = iota
	IntrinsicGt IntrinsicType = iota

	IntrinsicDup  IntrinsicType = iota
	IntrinsicSwap IntrinsicType = iota
	IntrinsicDrop IntrinsicType = iota
	IntrinsicOver IntrinsicType = iota
	IntrinsicRot  IntrinsicType = iota

	IntrinsicPuti IntrinsicType = iota
	IntrinsicPuts IntrinsicType = iota
	IntrinsicPutc IntrinsicType = iota

	IntrinsicDebug IntrinsicType = iota

	IntrinsicLoad8   IntrinsicType = iota
	IntrinsicStore8  IntrinsicType = iota
	IntrinsicLoad16  IntrinsicType = iota
	IntrinsicStore16 IntrinsicType = iota
	IntrinsicLoad32  IntrinsicType = iota
	IntrinsicStore32 IntrinsicType = iota
	IntrinsicLoad64  IntrinsicType = iota
	IntrinsicStore64 IntrinsicType = iota

	IntrinsicCount = iota
)

// TODO: how to assert global dicts have all key/value pairs?
// assert(IntrinsicCount == 4, "Unhandled intrinsic in WordToIntrinsic")
var WordToIntrinsic = map[string]IntrinsicType{
	"+": IntrinsicPlus,
	"-": IntrinsicMinus,
	"*": IntrinsicMul,
	"/": IntrinsicDiv,
	"%": IntrinsicMod,

	"<<": IntrinsicShl,
	">>": IntrinsicShr,
	"&":  IntrinsicBitAnd,
	"|":  IntrinsicBitOr,
	"^":  IntrinsicBitXor,

	"&&": IntrinsicLogicalAnd,
	"||": IntrinsicLogicalOr,
	"!":  IntrinsicLogicalNot,

	"=":  IntrinsicEq,
	"!=": IntrinsicNe,
	"<=": IntrinsicLe,
	">=": IntrinsicGe,
	"<":  IntrinsicLt,
	">":  IntrinsicGt,

	"dup":  IntrinsicDup,
	"swap": IntrinsicSwap,
	"drop": IntrinsicDrop,
	"over": IntrinsicOver,
	"rot":  IntrinsicRot,

	"puti": IntrinsicPuti,
	"puts": IntrinsicPuts,
	"putc": IntrinsicPutc,

	"???": IntrinsicDebug,

	"@8":  IntrinsicLoad8,
	"!8":  IntrinsicStore8,
	"@16": IntrinsicLoad16,
	"!16": IntrinsicStore16,
	"@32": IntrinsicLoad32,
	"!32": IntrinsicStore32,
	"@64": IntrinsicLoad64,
	"!64": IntrinsicStore64,
}

// assert(IntrinsicCount == 4, "Unhandled intrinsic in IntrinsicName")
var IntrinsicName = map[IntrinsicType]string{
	IntrinsicPlus:  "+",
	IntrinsicMinus: "-",
	IntrinsicMul:   "*",
	IntrinsicDiv:   "/",
	IntrinsicMod:   "%",

	IntrinsicShl:    "<<",
	IntrinsicShr:    ">>",
	IntrinsicBitAnd: "&",
	IntrinsicBitOr:  "|",
	IntrinsicBitXor: "^",

	IntrinsicEq: "=",
	IntrinsicNe: "!=",
	IntrinsicLe: "<=",
	IntrinsicGe: ">=",
	IntrinsicLt: "<",
	IntrinsicGt: ">",

	IntrinsicLogicalAnd: "&&",
	IntrinsicLogicalOr:  "||",
	IntrinsicLogicalNot: "!",

	IntrinsicDup:  "dup",
	IntrinsicSwap: "swap",
	IntrinsicDrop: "drop",
	IntrinsicOver: "over",
	IntrinsicRot:  "rot",

	IntrinsicPuti: "puti",
	IntrinsicPuts: "puts",
	IntrinsicPutc: "putc",

	IntrinsicDebug: "???",

	IntrinsicLoad8:   "@8",
	IntrinsicStore8:  "!8",
	IntrinsicLoad16:  "@16",
	IntrinsicStore16: "!16",
	IntrinsicLoad32:  "@32",
	IntrinsicStore32: "!32",
	IntrinsicLoad64:  "@64",
	IntrinsicStore64: "!64",
}

type BoolType int

const (
	BoolTrue  BoolType = iota
	BoolFalse BoolType = iota
)

var WordToBool = map[string]BoolType{
	"true":  BoolTrue,
	"false": BoolFalse,
}
var BoolName = map[BoolType]string{
	BoolTrue:  "true",
	BoolFalse: "false",
}

type OpType int

const (
	OpPushInt   OpType = iota
	OpPushBool  OpType = iota
	OpPushStr   OpType = iota
	OpIntrinsic OpType = iota
	OpIf        OpType = iota
	OpElse      OpType = iota
	OpEnd       OpType = iota
	OpWhile     OpType = iota
	OpDo        OpType = iota

	OpCount = iota
)

var OpName = map[OpType]string{
	OpPushInt:   "PUSH_INT",
	OpPushBool:  "PUSH_BOOL",
	OpPushStr:   "PUSH_STR",
	OpIntrinsic: "INTRINSIC",
	OpIf:        "IF",
	OpElse:      "ELSE",
	OpEnd:       "END",
	OpWhile:     "WHILE",
	OpDo:        "DO",
}

type Op struct {
	Typ     OpType
	Operand interface{}
	OpToken Token
}

func (op *Op) str(addr int) (s string) {
	var operand string

	assert(OpCount == 9, "Unhandled Op in Op.str()")
	switch op.Typ {
	case OpPushInt:
		operand = strconv.Itoa(op.Operand.(int))
	case OpPushBool:
		operand = BoolName[op.Operand.(BoolType)]
	case OpPushStr:
		operand = strconv.Itoa(op.Operand.(int))
	case OpIntrinsic:
		operand = IntrinsicName[op.Operand.(IntrinsicType)]
	case OpIf:
		operand = strconv.Itoa(op.Operand.(int))
	case OpElse:
		operand = strconv.Itoa(op.Operand.(int))
	case OpWhile:
		operand = ""
	case OpDo:
		operand = strconv.Itoa(op.Operand.(int))
	case OpEnd:
		operand = strconv.Itoa(op.Operand.(int))
	}

	s = fmt.Sprintf("%4d: %s %v", addr, OpName[op.Typ], operand)
	return
}

func Exit(exitcode int) {
	os.Exit(exitcode)
}

func CompilerFatal(loc *Location, msg string) {
	fmt.Printf("%s:%d:%d: ERROR: %s\n", loc.Filepath, loc.Line+1, loc.Column+1, msg)
}

func CompilerInfo(loc *Location, msg string) {
	fmt.Printf("%s:%d:%d: INFO: %s\n", loc.Filepath, loc.Line+1, loc.Column+1, msg)
}

func RuntimeFatal(loc *Location, msg string) {
	fmt.Printf("%s:%d:%d: RuntimeError: %s\n", loc.Filepath, loc.Line+1, loc.Column+1, msg)
	os.Exit(1)
}

type Block struct {
	Addr int
	Tok  Token
}

type Stack struct {
	Data []interface{}
}

func (s *Stack) push(x interface{}) {
	s.Data = append(s.Data, x)
	// fmt.Printf("INFO: stack after push(%d): %v\n", x, s.Data)
}

func (s *Stack) pop(token *Token) (x interface{}) {
	if len(s.Data) > 0 {
		x = s.Data[len(s.Data)-1]
		s.Data = s.Data[:len(s.Data)-1]
	} else {
		CompilerFatal(&token.Loc, "Stack underflow")
		Exit(1)
	}
	// fmt.Printf("INFO: stack after pop: %v\n", s.Data)
	return
}

func (s *Stack) top(token *Token) (x interface{}) {
	if len(s.Data) > 0 {
		x = s.Data[len(s.Data)-1]
	} else {
		CompilerFatal(&token.Loc, "Stack underflow")
		Exit(1)
	}

	return
}

func (s *Stack) size() int {
	return len(s.Data)
}

func read_file(fn string) (lines []string) {
	file, err := os.Open(fn)
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		if err = file.Close(); err != nil {
			log.Fatal(err)
		}
	}()

	reader := bufio.NewReader(file)
	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				break
			}

			log.Fatalf("read file line error: %v", err)
			return
		}
		line = strings.TrimRight(line, "\n")
		lines = append(lines, line)
	}

	return
}

const MemorySize = 640 * 1024

var Memory [MemorySize]byte
var MemPtr = 1 // Make 0 an invalid pointer

var Allocs = make(map[string]int)

var Consts = make(map[string]int)

var Names = make(map[string]Token)

type Lexer struct {
	Fn    string
	Loc   Location
	Lines []string
	Row   int
	Col   int
}

func const_eval(name_token *Token, tokens *[]Token) (value int) {
	const_stack := &Stack{}
	for _, token := range *tokens {
		switch token.Typ {
		case TokenInt:
			const_stack.push(token.Value.(int))
		case TokenWord:
			intrinsic, exists := WordToIntrinsic[token.Text]
			if exists {
				switch intrinsic {
				case IntrinsicPlus:
					b := const_stack.pop(&token).(int)
					a := const_stack.pop(&token).(int)
					const_stack.push(a + b)
				case IntrinsicMinus:
					b := const_stack.pop(&token).(int)
					a := const_stack.pop(&token).(int)
					const_stack.push(a - b)
				case IntrinsicMul:
					b := const_stack.pop(&token).(int)
					a := const_stack.pop(&token).(int)
					const_stack.push(a * b)
				case IntrinsicDiv:
					b := const_stack.pop(&token).(int)
					if b == 0 {
						CompilerFatal(&token.Loc, "Division by zero")
						Exit(1)
					}
					a := const_stack.pop(&token).(int)
					const_stack.push(a / b)
				case IntrinsicMod:
					b := const_stack.pop(&token).(int)
					if b == 0 {
						CompilerFatal(&token.Loc, "Division by zero")
						Exit(1)
					}
					a := const_stack.pop(&token).(int)
					const_stack.push(a % b)
				case IntrinsicShl:
					b := const_stack.pop(&token).(int)
					if b < 0 {
						CompilerFatal(&token.Loc, fmt.Sprintf("Negative shift amount in `<<`: %d", b))
						Exit(1)
					}
					a := const_stack.pop(&token).(int)
					const_stack.push(a << b)
				case IntrinsicShr:
					b := const_stack.pop(&token).(int)
					if b < 0 {
						CompilerFatal(&token.Loc, fmt.Sprintf("Negative shift amount in `>>`: %d", b))
						Exit(1)
					}
					a := const_stack.pop(&token).(int)
					const_stack.push(a >> b)

				default:
					CompilerFatal(
						&token.Loc,
						fmt.Sprintf(
							"Unexpected intrinsic in const-block compile-time "+
								"evaluation: %s. Supported: [+, -, *, /, %%, >>, <<]",
							token.Text,
						),
					)
					Exit(1)
				}
				continue
			}

			val, exists := Consts[token.Text]
			if exists {
				const_stack.push(val)
				continue
			}

			CompilerFatal(&token.Loc, fmt.Sprintf("Unsupported word in compile-time const-block evaluation: %s", token.Text))
			Exit(1)
		default:
			CompilerFatal(&token.Loc, fmt.Sprintf("Unsupported token in compile-time const-block evaluation: %s", token.Text))
			Exit(1)
		}
	}

	if const_stack.size() > 1 {
		CompilerFatal(&name_token.Loc, fmt.Sprintf("Unhandled data in compile-time const-block evaluation stack"))
		Exit(1)
	}

	value = const_stack.pop(name_token).(int)
	return
}

func (lx *Lexer) parse_const_block(token *Token, typ string) (tok Token, const_value int) {
	tok, end := lx.next_token()
	if end {
		CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name, but got nothing", typ))
		Exit(1)
	}
	if tok.Typ != TokenWord {
		CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name to be a word, but got %s", typ, tok.Text))
		Exit(1)
	}
	defined_token, exists := Names[tok.Text]
	if exists {
		CompilerFatal(&tok.Loc, fmt.Sprintf("Redefinition of word <%s>", tok.Text))
		CompilerInfo(&defined_token.Loc, "Previously defined here")
		Exit(1)
	}

	const_block := make([]Token, 0)
	for {
		tok, end := lx.next_token()
		if end {
			CompilerFatal(&token.Loc, fmt.Sprintf("Unexpected end while processing `%s` block", typ))
			Exit(1)
		}

		if tok.Typ == TokenKeyword && tok.Value.(KeywordType) == KeywordEnd {
			break
		}

		const_block = append(const_block, tok)
	}

	const_value = const_eval(&tok, &const_block)
	return
}

func (lx *Lexer) process_file(fn string) (tokens []Token) {
	lx.Fn = fn
	lx.Row = 0
	lx.Col = 0

	lx.Lines = read_file(lx.Fn)
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
			case KeywordConst:
				tok, const_value := lx.parse_const_block(&token, token.Text)

				Consts[tok.Text] = const_value
				Names[tok.Text] = tok
			case KeywordAlloc:
				tok, alloc_size := lx.parse_const_block(&token, token.Text)
				if alloc_size < 0 {
					CompilerFatal(&tok.Loc, fmt.Sprintf("Negative size for `alloc` block: %d", alloc_size))
					Exit(1)
				}

				Allocs[tok.Text] = MemPtr
				MemPtr += alloc_size
				Names[tok.Text] = tok
			}

		case TokenWord:
			intrinsic, exists := WordToIntrinsic[token.Text]
			if exists {
				token.Value = intrinsic
				tokens = append(tokens, token)
				continue
			}

			ptr, exists := Allocs[token.Text]
			if exists {
				token.Typ = TokenInt
				token.Value = ptr
				tokens = append(tokens, token)
				continue
			}

			val, exists := Consts[token.Text]
			if exists {
				token.Typ = TokenInt
				token.Value = val
				tokens = append(tokens, token)
				continue
			}

			CompilerFatal(&token.Loc, fmt.Sprintf("Unknown word: %s", token.Text))
			Exit(1)
		default:
			CompilerFatal(&token.Loc, "Unhandled Token.Typ in Lexer.process_file")
			Exit(1)
		}

	}
	return
}

func (lx *Lexer) ChopChar(data string, pos int) (b byte, escaped bool) {
	escaped = false
	if data[pos] == '\\' {
		escaped = true
		if pos+1 == len(data) {
			CompilerFatal(&lx.Loc, "Unexpected end of escaped char literal")
			Exit(1)
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
			Exit(1)
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
			Exit(1)
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
			CompilerFatal(&lx.Loc, fmt.Sprintf("Unexpecting end of char literal"))
			Exit(1)
		}
		if data[pos] != '\'' {
			CompilerFatal(&lx.Loc, fmt.Sprintf("Expecting to find closing ' for char literal, but got <%s>", string(data[pos])))
			Exit(1)
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
	assert(TokenCount == 6, "Unhandled Token in lex()")

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
		boolean, exists := WordToBool[word]
		if exists {
			// fmt.Printf("<%s> - is a boolean\n", word)
			token.Typ = TokenBool
			token.Value = boolean
			return
		}

		// check if word is an intrinsic
		// TODO: move to process_file maybe?
		// intrinsic, exists := WordToIntrinsic[word]
		// if exists {
		// 	// fmt.Printf("<%s> - is an intrinsic\n", word)
		// 	token.Typ = TokenWord
		// 	token.Value = intrinsic
		// 	return
		// }

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

func LoadFromMem(ptr int, size int) (value int) {
	value = 0
	n := 0
	for n < size {
		value = (value << 8) | int(Memory[ptr+n])
		n++
	}
	return
}

func StoreToMem(ptr int, value int, size int) {
	diff := size - 1
	for diff >= 0 {
		b := byte(value & 0xFF)
		Memory[ptr+diff] = b
		value >>= 8
		diff--
	}
	return
}

func compile(tokens []Token) (ops []Op) {
	assert(TokenCount == 6, "Unhandled Token in compile()")

	blocks := &Stack{}

	for _, token := range tokens {

		switch token.Typ {
		case TokenInt:
			ops = append(ops, Op{
				Typ:     OpPushInt,
				Operand: token.Value.(int),
				OpToken: token,
			})
		case TokenString:
			StringLiteralsBuffer = append(StringLiteralsBuffer, token.Value.(string))
			ops = append(ops, Op{
				Typ:     OpPushStr,
				Operand: len(StringLiteralsBuffer) - 1,
				OpToken: token,
			})
		case TokenChar:
			ops = append(ops, Op{
				Typ:     OpPushInt,
				Operand: token.Value.(int),
				OpToken: token,
			})
		case TokenBool:
			ops = append(ops, Op{
				Typ:     OpPushBool,
				Operand: token.Value.(BoolType),
				OpToken: token,
			})
		case TokenWord:
			name := token.Text

			_, exists := WordToIntrinsic[name]
			if exists {
				ops = append(ops, Op{
					Typ:     OpIntrinsic,
					Operand: token.Value.(IntrinsicType),
					OpToken: token,
				})
				continue
			}

			val, exists := Consts[name]
			if exists {
				ops = append(ops, Op{
					Typ:     OpPushInt,
					Operand: val,
					OpToken: token,
				})
				continue
			}

			ptr, exists := Allocs[name]
			if exists {
				ops = append(ops, Op{
					Typ:     OpPushInt,
					Operand: ptr,
					OpToken: token,
				})
				continue
			}

			CompilerFatal(&token.Loc, fmt.Sprintf("Unknown word %s: probably bug in next_token", token.Text))
			Exit(1)

		case TokenKeyword:
			kw_type := token.Value.(KeywordType)
			op := Op{OpToken: token}
			switch kw_type {
			case KeywordIf:
				op.Typ = OpIf
				blocks.push(Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case KeywordElse:
				if blocks.size() == 0 {
					CompilerFatal(&token.Loc, "Unexpected `end` found")
					Exit(1)
				}
				block := blocks.pop(&token).(Block)
				if block.Tok.Typ != TokenKeyword {
					CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but not `%s`. Probably bug in lex()", block.Tok.Text))
					Exit(1)
				}
				block_start_kw := block.Tok.Value.(KeywordType)
				switch block_start_kw {
				case KeywordIf:
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
				case KeywordElse:
					CompilerFatal(&token.Loc, fmt.Sprintf("`else` may only come after `if` block, but got `%s`. Probably bug in lex()", block.Tok.Text))
					Exit(1)
				case KeywordEnd:
					CompilerFatal(&token.Loc, fmt.Sprintf("`else` may only come after `if` block, but got `%s`. Probably bug in lex()", block.Tok.Text))
					Exit(1)
				case KeywordWhile:
					CompilerFatal(&token.Loc, fmt.Sprintf("`else` may only come after `if` block, but got `%s`. Probably bug in lex()", block.Tok.Text))
					Exit(1)
				default:
					CompilerFatal(&token.Loc, "Unhandled block start processing in compile() at KeywordElse")
					Exit(1)
				}

				op.Typ = OpElse
				blocks.push(Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case KeywordEnd:
				if blocks.size() == 0 {
					CompilerFatal(&token.Loc, "Unexpected `end` found")
					Exit(1)
				}
				block := blocks.pop(&token).(Block)
				if block.Tok.Typ != TokenKeyword {
					CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but not `%s`. Probably bug in lex()", block.Tok.Text))
					Exit(1)
				}
				block_start_kw := block.Tok.Value.(KeywordType)
				op.Operand = 1
				switch block_start_kw {
				case KeywordIf:
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
				case KeywordElse:
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
				case KeywordWhile:
					CompilerFatal(&block.Tok.Loc, "`while` block must contain `do` before `end`")
					Exit(1)
				case KeywordDo:
					op.Operand = ops[block.Addr].Operand.(int) + (block.Addr - len(ops))
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
				case KeywordEnd:
					CompilerFatal(&token.Loc, fmt.Sprintf("`end` may only close `if-else` or `while-do` blocks, but got `%s`. Probably bug in lex()", block.Tok.Text))
					Exit(1)
				default:
					CompilerFatal(&token.Loc, "Unhandled block start processing in compile()")
					Exit(1)
				}
				op.Typ = OpEnd
				ops = append(ops, op)
			case KeywordWhile:
				op.Typ = OpWhile
				blocks.push(Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case KeywordDo:
				if blocks.size() == 0 {
					CompilerFatal(&token.Loc, "Unexpected `do` found")
					Exit(1)
				}
				block := blocks.pop(&token).(Block)
				if block.Tok.Typ != TokenKeyword {
					CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but not `%s`. Probably bug in lex()", block.Tok.Text))
					Exit(1)
				}
				if block.Tok.Value.(KeywordType) != KeywordWhile {
					CompilerFatal(&token.Loc, fmt.Sprintf("`do` may come only inside `while` block, but not `%s`. Probably bug in lex()", block.Tok.Text))
					Exit(1)
				}
				op.Typ = OpDo
				op.Operand = block.Addr - len(ops) // save relative address of `while`
				blocks.push(Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			default:
				CompilerFatal(&token.Loc, fmt.Sprintf("Unhandled KewordType handling in compile(): %s", token.Text))
				Exit(1)
			}
		default:
			CompilerFatal(&token.Loc, fmt.Sprintf("ERROR: Unhandled token: %s\n", token.Text))
			Exit(1)
		}
	}

	if len(blocks.Data) > 0 {
		top := blocks.Data[len(blocks.Data)-1].(Block)
		CompilerFatal(&top.Tok.Loc, fmt.Sprintf("Unclosed %s-block", top.Tok.Text))
		Exit(1)
	}
	return
}

func interprete(ops []Op, debug bool) {
	stack := &Stack{}

	if debug {
		for addr, op := range ops {
			fmt.Println(op.str(addr))
		}
		fmt.Println("---------------------------------")
	}

	assert(OpCount == 9, "Unhandled Op in interprete()")
	addr := 0
	for addr < len(ops) {

		op := ops[addr]
		// fmt.Printf("Process addr=%d stack=%v\n", addr, stack.Data)

		switch op.Typ {
		case OpPushInt:
			n := op.Operand.(int)
			stack.push(n)
			addr++
		case OpPushBool:
			switch op.Operand {
			case BoolFalse:
				stack.push(false)
			case BoolTrue:
				stack.push(true)
			}
			addr++
		case OpPushStr:
			ptr := op.Operand.(int)
			stack.push(ptr)
			addr++
		case OpIf:
			top := stack.pop(&op.OpToken).(bool)
			if top {
				addr++
			} else {
				addr += op.Operand.(int)
			}
		case OpElse:
			addr += op.Operand.(int)
		case OpEnd:
			addr += op.Operand.(int)
		case OpWhile:
			addr++
		case OpDo:
			top := stack.pop(&op.OpToken).(bool)
			if top {
				addr++
			} else {
				addr += op.Operand.(int)
			}
		case OpIntrinsic:
			assert(IntrinsicCount == 36, "Unhandled intrinsic in interprete()")
			switch op.Operand {
			case IntrinsicPlus:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) + b.(int))
			case IntrinsicMinus:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) - b.(int))
			case IntrinsicMul:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) * b.(int))
			case IntrinsicDiv:
				b := stack.pop(&op.OpToken)
				b_arg := b.(int)
				if b_arg == 0 {
					RuntimeFatal(&op.OpToken.Loc, "Division by zero")
				}
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) / b_arg)
			case IntrinsicMod:
				b := stack.pop(&op.OpToken)
				b_arg := b.(int)
				if b_arg == 0 {
					RuntimeFatal(&op.OpToken.Loc, "Division by zero")
				}
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) % b_arg)
			case IntrinsicShl:
				b := stack.pop(&op.OpToken)
				b_arg := b.(int)
				if b_arg < 0 {
					RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Negative shift amount in `<<`: %d", b_arg))
				}
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) << b_arg)
			case IntrinsicShr:
				b := stack.pop(&op.OpToken)
				b_arg := b.(int)
				if b_arg < 0 {
					RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Negative shift amount in `>>`: %d", b_arg))
				}
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) >> b_arg)
			case IntrinsicLogicalAnd:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(bool) && b.(bool))
			case IntrinsicLogicalOr:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(bool) || b.(bool))
			case IntrinsicLogicalNot:
				x := stack.pop(&op.OpToken)
				stack.push(!x.(bool))
			case IntrinsicBitAnd:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) & b.(int))
			case IntrinsicBitOr:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) | b.(int))
			case IntrinsicBitXor:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) ^ b.(int))
			case IntrinsicDup:
				x := stack.top(&op.OpToken)
				stack.push(x)
			case IntrinsicSwap:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(b)
				stack.push(a)
			case IntrinsicDrop:
				_ = stack.pop(&op.OpToken)
			case IntrinsicOver:
				x := stack.Data[len(stack.Data)-2]
				stack.push(x)
			case IntrinsicRot:
				c := stack.pop(&op.OpToken)
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(b)
				stack.push(c)
				stack.push(a)
			case IntrinsicEq:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) == b.(int))
			case IntrinsicNe:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) != b.(int))
			case IntrinsicLe:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) <= b.(int))
			case IntrinsicGe:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) >= b.(int))
			case IntrinsicLt:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) < b.(int))
			case IntrinsicGt:
				b := stack.pop(&op.OpToken)
				a := stack.pop(&op.OpToken)
				stack.push(a.(int) > b.(int))
			case IntrinsicPuti:
				x := stack.pop(&op.OpToken)
				fmt.Print(x)
			case IntrinsicPutc:
				x := stack.pop(&op.OpToken)
				fmt.Print(string(byte(x.(int))))
			case IntrinsicPuts:
				x := stack.pop(&op.OpToken)
				index := x.(int)
				str := StringLiteralsBuffer[index]
				fmt.Print(str)
			case IntrinsicDebug:
				fmt.Printf("\tMem: %v\tStack: %v\n", Memory[:MemPtr], stack.Data)
			case IntrinsicLoad8:
				x := stack.pop(&op.OpToken)
				ptr := x.(int)
				val := LoadFromMem(ptr, 1)
				stack.push(val)
			case IntrinsicStore8:
				ptr := stack.pop(&op.OpToken).(int)
				x := stack.pop(&op.OpToken).(int)
				StoreToMem(ptr, x, 1)
			case IntrinsicLoad16:
				x := stack.pop(&op.OpToken)
				ptr := x.(int)
				value := LoadFromMem(ptr, 2)
				stack.push(value)
			case IntrinsicStore16:
				ptr := stack.pop(&op.OpToken).(int)
				x := stack.pop(&op.OpToken).(int)
				StoreToMem(ptr, x, 2)
			case IntrinsicLoad32:
				x := stack.pop(&op.OpToken)
				ptr := x.(int)
				value := LoadFromMem(ptr, 4)
				stack.push(value)
			case IntrinsicStore32:
				ptr := stack.pop(&op.OpToken).(int)
				x := stack.pop(&op.OpToken).(int)
				StoreToMem(ptr, x, 4)
			case IntrinsicLoad64:
				x := stack.pop(&op.OpToken)
				ptr := x.(int)
				value := LoadFromMem(ptr, 8)
				stack.push(value)
			case IntrinsicStore64:
				ptr := stack.pop(&op.OpToken).(int)
				x := stack.pop(&op.OpToken).(int)
				StoreToMem(ptr, x, 8)
			default:
				CompilerFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled intrinsic: %s", op.OpToken.Text))
			}
			addr++
		}
	}

	if debug {
		fmt.Println("---------------------------------")
		fmt.Printf("Allocated: %d byte(s) total\n", MemPtr)
		fmt.Printf("Memory: %v\n", Memory[:MemPtr])
		fmt.Println("---------------------------------")
	}
}

func main() {
	debugFlag := flag.Bool("debug", false, "run in debug mode")
	flag.Parse()

	gorth_script := flag.Args()[0]

	lx := Lexer{}
	interprete(compile(lx.process_file(gorth_script)), *debugFlag)
}
