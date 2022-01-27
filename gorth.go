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

func reverseMap(in map[interface{}]string) (result map[string]interface{}) {
	for key, value := range in {
		result[value] = key
	}
	return
}

type TokenType int

const (
	TokenInt  TokenType = iota
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
	IntrinsicPrint IntrinsicType = iota
)

var WordToIntrinsic = map[string]IntrinsicType{
	"+":     IntrinsicPlus,
	"-":     IntrinsicMinus,
	"*":     IntrinsicMul,
	"print": IntrinsicPrint,
}
var IntrinsicName = map[IntrinsicType]string{
	IntrinsicPlus:  "+",
	IntrinsicMinus: "-",
	IntrinsicMul:   "*",
	IntrinsicPrint: "print",
}

type OpType int

const (
	OpPushInt OpType = iota
	OpIntrinsic
)

var OpName = map[OpType]string{
	OpPushInt:   "PUSH_INT",
	OpIntrinsic: "INTRINSIC",
}

type Op struct {
	Typ     OpType
	Operand interface{}
	OpToken Token
}

func (op *Op) str() (s string) {
	var operand string

	switch op.Typ {
	case OpPushInt:
		operand = strconv.Itoa(op.Operand.(int))
	case OpIntrinsic:
		operand = IntrinsicName[op.Operand.(IntrinsicType)]
	}

	s = fmt.Sprintf("%s %v", OpName[op.Typ], operand)
	return
}

type Stack struct {
	Data []int
}

func (s *Stack) push(x int) {
	s.Data = append(s.Data, x)
	// fmt.Printf("INFO: stack after push(%d): %v\n", x, s.Data)
}

func (s *Stack) pop() (x int) {
	x = s.Data[len(s.Data)-1]
	s.Data = s.Data[:len(s.Data)-1]
	// fmt.Printf("INFO: stack after pop: %v\n", s.Data)
	return
}

func (s *Stack) top() (x int) {
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
	for _, token := range tokens {

		switch token.Typ {
		case TokenInt:
			ops = append(ops, Op{
				Typ:     OpPushInt,
				Operand: token.Value.(int),
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

	for _, op := range ops {
		switch op.Typ {
		case OpPushInt:
			n := op.Operand.(int)
			stack.push(n)
		case OpIntrinsic:
			switch op.Operand {
			case IntrinsicPlus:
				b := stack.pop()
				a := stack.pop()
				stack.push(a + b)
			case IntrinsicMinus:
				b := stack.pop()
				a := stack.pop()
				stack.push(a - b)
			case IntrinsicMul:
				b := stack.pop()
				a := stack.pop()
				stack.push(a * b)
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
