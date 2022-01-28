package main

import (
	"flag"
	"fmt"
	"io/ioutil"
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
}

type IntrinsicType int

const (
	IntrinsicPlus  IntrinsicType = iota
	IntrinsicMinus IntrinsicType = iota
	IntrinsicMul   IntrinsicType = iota

	IntrinsicEq IntrinsicType = iota

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
	"+":     IntrinsicPlus,
	"-":     IntrinsicMinus,
	"*":     IntrinsicMul,
	"=":     IntrinsicEq,

	"dup":   IntrinsicDup,
	"swap":  IntrinsicSwap,
	"drop":  IntrinsicDrop,
	"over":  IntrinsicOver,
	"rot":   IntrinsicRot,

	"print": IntrinsicPrint,
}

// assert(IntrinsicCount == 4, "Unhandled intrinsic in IntrinsicName")
var IntrinsicName = map[IntrinsicType]string{
	IntrinsicPlus:  "+",
	IntrinsicMinus: "-",
	IntrinsicMul:   "*",
	IntrinsicEq:    "=",
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

type Stack struct {
	Data []interface{}
}

func (s *Stack) push(x interface{}) {
	s.Data = append(s.Data, x)
	// fmt.Printf("INFO: stack after push(%d): %v\n", x, s.Data)
}

func (s *Stack) pop() (x interface{}) {
	x = s.Data[len(s.Data)-1]
	s.Data = s.Data[:len(s.Data)-1]
	// fmt.Printf("INFO: stack after pop: %v\n", s.Data)
	return
}

func (s *Stack) top() (x interface{}) {
	x = s.Data[len(s.Data)-1]
	return
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func read_file(fn string) (data string) {
	file, err := os.Open(fn)
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		if err = file.Close(); err != nil {
			log.Fatal(err)
		}
	}()

	bytes, err := ioutil.ReadAll(file)
	data = string(bytes)

	return
}

func lex(data string) (tokens []Token) {
	start := 0

	assert(TokenCount == 3, "Unhandled Token in lex()")
	for start < len(data) {
		if strings.IndexByte(" \n", data[start]) != -1 {
			start++
			continue
		}
		i := start
		for i < len(data) && strings.IndexByte(" \n", data[i]) == -1 {
			i++
		}
		word := data[start:i]
		start = i

		// check if word is int literal
		number, err := strconv.Atoi(word)
		if err == nil {
			tokens = append(tokens, Token{Typ: TokenInt, Text: word, Value: number})
			continue
		}

		// check if word is boolean literal
		boolean, exists := WordToBool[word]
		if exists {
			tokens = append(tokens, Token{Typ: TokenBool, Text: word, Value: boolean})
			continue
		}

		// check if word is an intrinsic (+-* or print)
		intrinsic, exists := WordToIntrinsic[word]
		if exists {
			tokens = append(tokens, Token{Typ: TokenWord, Text: word, Value: intrinsic})
			continue
		}

		log.Fatal(fmt.Printf("ERROR: unhandled word: %s\n", word))
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
			log.Fatal(fmt.Sprintf("ERROR: Unhandled token: %d\n", token.Typ))
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
			assert(IntrinsicCount == 10, "Unhandled intrinsic in interprete()")
			switch op.Operand {
			case IntrinsicPlus:
				b := stack.pop().(int)
				a := stack.pop().(int)
				stack.push(a + b)
			case IntrinsicMinus:
				b := stack.pop().(int)
				a := stack.pop().(int)
				stack.push(a - b)
			case IntrinsicMul:
				b := stack.pop().(int)
				a := stack.pop().(int)
				stack.push(a * b)
			case IntrinsicDup:
				x := stack.top()
				stack.push(x)
			case IntrinsicSwap:
				b := stack.pop()
				a := stack.pop()
				stack.push(b)
				stack.push(a)
			case IntrinsicDrop:
				_ = stack.pop()
			case IntrinsicOver:
				x := stack.Data[len(stack.Data)-2]
				stack.push(x)
			case IntrinsicRot:
				c := stack.pop()
				b := stack.pop()
				a := stack.pop()
				stack.push(b)
				stack.push(c)
				stack.push(a)
			case IntrinsicEq:
				b := stack.pop().(int)
				a := stack.pop().(int)
				stack.push(a == b)
			case IntrinsicPrint:
				x := stack.pop()
				fmt.Println(x)
			default:
				log.Fatal(fmt.Sprintf("ERROR: unhandled intrinsic: %v", op.Operand.(IntrinsicType)))
			}
		}
	}
}

func main() {
	debugFlag := flag.Bool("debug", false, "run in debug mode")
	flag.Parse()

	gorth_script := flag.Args()[0]

	interprete(compile(lex(read_file(gorth_script))), *debugFlag)
}
