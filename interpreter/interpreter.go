package interpreter

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/vm"
	"fmt"
	"os"
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

func (i *Interpreter) Run(fn string) vm.ExitCodeType {
	tokens := i.lx.ProcessFile(fn, []string{}, &i.imp)
	ops := i.vm.Compile(fn, tokens, i.args)
	return i.vm.Interprete(ops, i.args)
}

func (i *Interpreter) RunDebug(fn string, di *vm.DebugInterface) {
	tokens := i.lx.ProcessFile(fn, []string{}, &i.imp)
	ops := i.vm.Compile(fn, tokens, i.args)
	go i.vm.InterpreteDebug(ops, i.args, di)
}

func (i *Interpreter) ProcessExit(exit_code vm.ExitCodeType) {
	if exit_code.Code != 0 {
		fmt.Fprintf(os.Stderr, "Script finished with exit code %d", exit_code.Code)
		if len(exit_code.Msg) > 0 {
			fmt.Fprintf(os.Stderr, ": %s", exit_code.Msg)
		}
		fmt.Fprintln(os.Stderr)
	}
	os.Exit(int(exit_code.Code))
}
