package interpreter

import (
	"GoStudy/Gorth/interpreter/lexer"
	"GoStudy/Gorth/interpreter/vm"
)

type Interpreter struct {
	lx lexer.Lexer
	vm vm.VM
}

func InitInterpreter() *Interpreter {
	i := &Interpreter{
		lx: lexer.Lexer{},
		vm: *vm.InitVM(),
	}
	return i
}

func (i *Interpreter) Run(fn string, debug bool) {
	tokens := i.lx.ProcessFile(fn)
	ops := i.vm.Compile(tokens)
	i.vm.Interprete(ops, debug)
}
