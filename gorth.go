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

	KeywordCount = iota
)

var WordToKeyword = map[string]KeywordType{
	"if":    KeywordIf,
	"else":  KeywordElse,
	"end":   KeywordEnd,
	"while": KeywordWhile,
	"do":    KeywordDo,
}
var KeywordName = map[KeywordType]string{
	KeywordIf:    "if",
	KeywordElse:  "else",
	KeywordEnd:   "end",
	KeywordWhile: "while",
	KeywordDo:    "do",
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

func CompilerFatal(loc *Location, msg string) {
	fmt.Printf("%s:%d:%d: ERROR: %s\n", loc.Filepath, loc.Line+1, loc.Column+1, msg)
	os.Exit(1)
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
	}
	// fmt.Printf("INFO: stack after pop: %v\n", s.Data)
	return
}

func (s *Stack) top(token *Token) (x interface{}) {
	if len(s.Data) > 0 {
		x = s.Data[len(s.Data)-1]
	} else {
		CompilerFatal(&token.Loc, "Stack underflow")
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

type Lexer struct {
	Fn  string
	Loc Location
}

func (lx *Lexer) process_file(fn string) (tokens []Token) {
	lx.Fn = fn

	lines := read_file(lx.Fn)
	tokens = lx.lex(lines)
	return
}

func (lx *Lexer) ChopChar(data string, pos int) (b byte, escaped bool) {
	escaped = false
	if data[pos] == '\\' {
		escaped = true
		if pos+1 == len(data) {
			CompilerFatal(&lx.Loc, "Unexpected end of escaped char literal")
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
			CompilerFatal(&lx.Loc, fmt.Sprintf("Unknown escape character: %s", data[pos:pos+2]))
		}
	} else {
		b = data[pos]
	}
	return
}

func (lx *Lexer) ChopWord(data string, line int, pos int) (word string, comment bool) {
	comment = false
	for pos < len(data) {
		if strings.IndexByte(" \t", data[pos]) != -1 {
			pos++
			continue
		} else {
			break
		}
	}

	lx.Loc = Location{Filepath: lx.Fn, Line: line, Column: pos}

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
		}
		if data[pos] != '\'' {
			CompilerFatal(&lx.Loc, fmt.Sprintf("Expecting to find closing ' for char literal, but got <%s>", string(data[pos])))
		}
		word = "'" + string(b) + "'"
		pos++
	default:
		for pos < len(data) && data[pos] != ' ' {
			if data[pos] == ' ' {
				break
			}
			if pos < len(data)-1 && data[pos:pos+2] == "//" {
				comment = true
				break
			}
			pos++
		}
		word = data[start:pos]
	}

	return
}

func (lx *Lexer) lex(lines []string) (tokens []Token) {
	assert(TokenCount == 6, "Unhandled Token in lex()")
	for line, data := range lines {

		pos := 0
		for pos < len(data) {
			word, comment := lx.ChopWord(data, line, pos)
			if comment {
				break
			}

			pos = lx.Loc.Column + len(word) + 1
			token := Token{Text: word, Loc: lx.Loc}

			// check if word is string literal
			if word[0] == '"' {
				token.Typ = TokenString
				token.Value = word[1 : len(word)-1]
				tokens = append(tokens, token)
				continue
			}

			// check if word is char literal
			if word[0] == '\'' {
				token.Typ = TokenChar
				token.Value = int(word[1])
				tokens = append(tokens, token)
				continue
			}

			// check if word is int literal
			number, err := strconv.Atoi(word)
			if err == nil {
				token.Typ = TokenInt
				token.Value = number
				tokens = append(tokens, token)
				continue
			}

			// check if word is boolean literal
			boolean, exists := WordToBool[word]
			if exists {
				token.Typ = TokenBool
				token.Value = boolean
				tokens = append(tokens, token)
				continue
			}

			// check if word is an intrinsic
			intrinsic, exists := WordToIntrinsic[word]
			if exists {
				token.Typ = TokenWord
				token.Value = intrinsic
				tokens = append(tokens, token)
				continue
			}

			// check if word is keyword
			keyword, exists := WordToKeyword[word]
			if exists {
				token.Typ = TokenKeyword
				token.Value = keyword
				tokens = append(tokens, token)
				continue
			}

			CompilerFatal(&token.Loc, fmt.Sprintf("Unknown word: %s", word))
		}
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
			ops = append(ops, Op{
				Typ:     OpIntrinsic,
				Operand: token.Value.(IntrinsicType),
				OpToken: token,
			})
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
				}
				block := blocks.pop(&token).(Block)
				if block.Tok.Typ != TokenKeyword {
					CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but not `%s`. Probably bug in lex()", block.Tok.Text))
				}
				block_start_kw := block.Tok.Value.(KeywordType)
				switch block_start_kw {
				case KeywordIf:
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
				case KeywordElse:
					CompilerFatal(&token.Loc, fmt.Sprintf("`else` may only come after `if` block, but got `%s`. Probably bug in lex()", block.Tok.Text))
				case KeywordEnd:
					CompilerFatal(&token.Loc, fmt.Sprintf("`else` may only come after `if` block, but got `%s`. Probably bug in lex()", block.Tok.Text))
				case KeywordWhile:
					CompilerFatal(&token.Loc, fmt.Sprintf("`else` may only come after `if` block, but got `%s`. Probably bug in lex()", block.Tok.Text))
				default:
					CompilerFatal(&token.Loc, "Unhandled block start processing in compile() at KeywordElse")
				}

				op.Typ = OpElse
				blocks.push(Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case KeywordEnd:
				if blocks.size() == 0 {
					CompilerFatal(&token.Loc, "Unexpected `end` found")
				}
				block := blocks.pop(&token).(Block)
				if block.Tok.Typ != TokenKeyword {
					CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but not `%s`. Probably bug in lex()", block.Tok.Text))
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
				case KeywordDo:
					op.Operand = ops[block.Addr].Operand.(int) + (block.Addr - len(ops))
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
				case KeywordEnd:
					CompilerFatal(&token.Loc, fmt.Sprintf("`end` may only close `if-else` or `while-do` blocks, but got `%s`. Probably bug in lex()", block.Tok.Text))
				default:
					CompilerFatal(&token.Loc, "Unhandled block start processing in compile()")
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
				}
				block := blocks.pop(&token).(Block)
				if block.Tok.Typ != TokenKeyword {
					CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but not `%s`. Probably bug in lex()", block.Tok.Text))
				}
				if block.Tok.Value.(KeywordType) != KeywordWhile {
					CompilerFatal(&token.Loc, fmt.Sprintf("`do` may come only inside `while` block, but not `%s`. Probably bug in lex()", block.Tok.Text))
				}
				op.Typ = OpDo
				op.Operand = block.Addr - len(ops) // save relative address of `while`
				blocks.push(Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			default:
				CompilerFatal(&token.Loc, fmt.Sprintf("Unhandled KewordType handling in compile(): %s", token.Text))
			}
		default:
			CompilerFatal(&token.Loc, fmt.Sprintf("ERROR: Unhandled token: %s\n", token.Text))
		}
	}

	if len(blocks.Data) > 0 {
		top := blocks.Data[len(blocks.Data)-1].(Block)
		CompilerFatal(&top.Tok.Loc, fmt.Sprintf("Unclosed %s-block", top.Tok.Text))
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
			assert(IntrinsicCount == 27, "Unhandled intrinsic in interprete()")
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
			default:
				CompilerFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled intrinsic: %s", op.OpToken.Text))
			}
			addr++
		}
	}
}

func main() {
	debugFlag := flag.Bool("debug", false, "run in debug mode")
	flag.Parse()

	gorth_script := flag.Args()[0]

	lx := Lexer{}
	interprete(compile(lx.process_file(gorth_script)), *debugFlag)
}
