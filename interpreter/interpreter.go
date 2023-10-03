package interpreter

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"
	"fmt"
	"os"
)

type Interpreter struct {
	lx   lexer.Lexer
	vm   vm.VM
	c    vm.Compiler
	args []string
	imp  lexer.Importer
}

func InitInterpreter(arguments []string, pkg_dir string) *Interpreter {
	i := &Interpreter{
		lx:   lexer.Lexer{},
		vm:   *vm.InitVM(),
		c:    *vm.NewCompiler(),
		args: arguments,
		imp: lexer.Importer{
			Paths:    []string{pkg_dir},
			Included: make(map[string]bool),
		},
	}
	return i
}

func (i *Interpreter) Prepare(fn string) {
	tokens := i.lx.ProcessFile(fn, []string{}, &i.imp)

	i.vm.PreprocessTokens(&tokens)
	err := i.c.CompileTokens(&tokens, &i.vm.Ctx)
	if err != nil {
		fmt.Fprint(os.Stderr, err.Error())
		utils.Exit(1)
	}
}

func (i *Interpreter) Run(fn string) vm.ExitCodeType {
	i.Prepare(fn)
	return i.vm.Interprete(i.c.Ops, i.args)
}

func (i *Interpreter) RunDebug(fn string, di *vm.DebugInterface) {
	i.Prepare(fn)
	go i.vm.InterpreteDebug(i.c.Ops, i.args, di)
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
