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

func show_err_and_exit(err error) {
	fmt.Fprint(os.Stderr, err.Error())
	utils.Exit(1)
}

func InitInterpreter(arguments []string, pkg_dir string, s *vm.Settings) *Interpreter {
	i := &Interpreter{
		lx:   lexer.Lexer{},
		vm:   *vm.InitVM(s),
		c:    *vm.NewCompiler(s.Debug),
		args: arguments,
		imp: *lexer.NewImporter(pkg_dir, s.IncludePaths),
	}

	i.imp.Paths = append(i.imp.Paths, s.IncludePaths...)
	return i
}

func (i *Interpreter) Prepare(fn string) {
	th, err := i.lx.ProcessFile(fn, []string{}, &i.imp)
	if err != nil {
		show_err_and_exit(err)
	}

	i.vm.PreprocessTokens(th)
	err = i.c.CompileTokens(th, &i.vm.Ctx)
	if err != nil {
		show_err_and_exit(err)
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
