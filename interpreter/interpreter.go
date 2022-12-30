package interpreter

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/vm"
)

type Interpreter struct {
	lx   lexer.Lexer
	vm   vm.VM
	args []string
	imp  lexer.Importer
}

func InitInterpreter(arguments []string) *Interpreter {
	i := &Interpreter{
		lx:   lexer.Lexer{},
		vm:   *vm.InitVM(),
		args: arguments,
		imp: lexer.Importer{
			Paths:    []string{"."},
			Included: make(map[string]bool),
		},
	}
	return i
}

func (i *Interpreter) Run(fn string, debug bool) {
	tokens := i.lx.ProcessFile(fn, []string{}, &i.imp)
	ops := i.vm.Compile(fn, tokens, i.args)
	i.vm.Interprete(ops, i.args, debug)
}
