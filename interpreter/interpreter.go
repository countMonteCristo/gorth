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

func InitInterpreter(arguments []string, pkg_dir string) *Interpreter {
	i := &Interpreter{
		lx:   lexer.Lexer{},
		vm:   *vm.InitVM(),
		args: arguments,
		imp: lexer.Importer{
			Paths:    []string{pkg_dir},
			Included: make(map[string]bool),
		},
	}
	return i
}

func (i *Interpreter) Run(fn string) {
	tokens := i.lx.ProcessFile(fn, []string{}, &i.imp)
	ops := i.vm.Compile(fn, tokens, i.args)
	i.vm.Interprete(ops, i.args)
}

func (i *Interpreter) RunDebug(fn string, cmd <-chan string, resp chan<- string) {
	tokens := i.lx.ProcessFile(fn, []string{}, &i.imp)
	ops := i.vm.Compile(fn, tokens, i.args)
	go i.vm.InterpreteDebug(ops, i.args, cmd, resp)
}
