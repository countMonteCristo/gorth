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
	TokenInt  TokenType = iota
	TokenBool TokenType = iota
	TokenWord TokenType = iota

	TokenCount = iota
)

type Token struct {
	Typ   TokenType
	Text  string
	Value interface{}
	Loc   Location
}

type IntrinsicType int

const (
	IntrinsicPlus  IntrinsicType = iota
	IntrinsicMinus IntrinsicType = iota
	IntrinsicMul   IntrinsicType = iota

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

	IntrinsicPrint IntrinsicType = iota

	IntrinsicCount = iota
)

// TODO: how to assert global dicts have all key/value pairs?
// assert(IntrinsicCount == 4, "Unhandled intrinsic in WordToIntrinsic")
var WordToIntrinsic = map[string]IntrinsicType{
	"+": IntrinsicPlus,
	"-": IntrinsicMinus,
	"*": IntrinsicMul,

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

	"print": IntrinsicPrint,
}

// assert(IntrinsicCount == 4, "Unhandled intrinsic in IntrinsicName")
var IntrinsicName = map[IntrinsicType]string{
	IntrinsicPlus:  "+",
	IntrinsicMinus: "-",
	IntrinsicMul:   "*",

	IntrinsicEq: "=",
	IntrinsicNe: "!=",
	IntrinsicLe: "<=",
	IntrinsicGe: ">=",
	IntrinsicLt: "<",
	IntrinsicGt: ">",

	IntrinsicDup:   "dup",
	IntrinsicSwap:  "swap",
	IntrinsicDrop:  "drop",
	IntrinsicOver:  "over",
	IntrinsicRot:   "rot",
	IntrinsicPrint: "print",
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
	OpIntrinsic OpType = iota

	OpCount = iota
)

var OpName = map[OpType]string{
	OpPushInt:   "PUSH_INT",
	OpPushBool:  "PUSH_BOOL",
	OpIntrinsic: "INTRINSIC",
}

type Op struct {
	Typ     OpType
	Operand interface{}
	OpToken Token
}

func (op *Op) str() (s string) {
	var operand string

	assert(OpCount == 3, "Unhandled Op in Op.str()")
	switch op.Typ {
	case OpPushInt:
		operand = strconv.Itoa(op.Operand.(int))
	case OpPushBool:
		operand = BoolName[op.Operand.(BoolType)]
	case OpIntrinsic:
		operand = IntrinsicName[op.Operand.(IntrinsicType)]
	}

	s = fmt.Sprintf("%s %v", OpName[op.Typ], operand)
	return
}

func CompilerFatal(loc *Location, msg string) {
	fmt.Printf("%s:%d:%d: ERROR: %s\n", loc.Filepath, loc.Line+1, loc.Column+1, msg)
	os.Exit(1)
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

func check(e error) {
	if e != nil {
		panic(e)
	}
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

func lex(lines []string, filepath string) (tokens []Token) {
	assert(TokenCount == 3, "Unhandled Token in lex()")
	for line_num, data := range lines {
		// fmt.Printf("Check line: `%s`\n", data)

		start := 0
		for start < len(data) {

			if data[start] == ' ' {
				start++
				continue
			}
			i := start
			for i < len(data) && data[i] != ' ' {
				i++
			}
			word := data[start:i]

			token := Token{Text: word, Loc: Location{Filepath: filepath, Line: line_num, Column: start}}
			start = i

			// fmt.Printf("Check word: `%s`\n", word)

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

			CompilerFatal(&token.Loc, fmt.Sprintf("Unknown word: %s", word))
		}
	}

	return
}

func compile(tokens []Token) (ops []Op) {
	assert(TokenCount == 3, "Unhandled Token in compile()")
	for _, token := range tokens {

		switch token.Typ {
		case TokenInt:
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
		default:
			CompilerFatal(&token.Loc, fmt.Sprintf("ERROR: Unhandled token: %s\n", token.Text))
		}
	}
	return
}

func interprete(ops []Op, debug bool) {
	stack := &Stack{}

	if debug {
		for _, op := range ops {
			fmt.Println(op.str())
		}
		fmt.Println("---------------------------------")
	}

	assert(OpCount == 3, "Unhandled Op in interprete()")
	for _, op := range ops {
		switch op.Typ {
		case OpPushInt:
			n := op.Operand.(int)
			stack.push(n)
		case OpPushBool:
			switch op.Operand {
			case BoolFalse:
				stack.push(false)
			case BoolTrue:
				stack.push(true)
			}
		case OpIntrinsic:
			assert(IntrinsicCount == 15, "Unhandled intrinsic in interprete()")
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
			case IntrinsicPrint:
				x := stack.pop(&op.OpToken)
				fmt.Println(x)
			default:
				CompilerFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled intrinsic: %s", op.OpToken.Text))
			}
		}
	}
}

func main() {
	debugFlag := flag.Bool("debug", false, "run in debug mode")
	flag.Parse()

	gorth_script := flag.Args()[0]

	interprete(compile(lex(read_file(gorth_script), gorth_script)), *debugFlag)
}
