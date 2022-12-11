package interpreter

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/vm"
)

type Interpreter struct {
	lx   lexer.Lexer
	vm   vm.VM
	args []string
}

func InitInterpreter(arguments []string) *Interpreter {
	i := &Interpreter{
		lx:   lexer.Lexer{},
		vm:   *vm.InitVM(),
		args: arguments,
	}
	return i
}

func (i *Interpreter) Run(fn string, debug bool) {
	tokens := i.lx.ProcessFile(fn)
	ops := i.vm.Compile(tokens)
	i.vm.Interprete(ops, i.args, debug)
}
