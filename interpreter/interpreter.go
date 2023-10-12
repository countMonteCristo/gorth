package interpreter

import (
	"Gorth/interpreter/compiler"
	"Gorth/interpreter/debugger"
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/typechecker"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"
	"fmt"
	"os"
)

type Interpreter struct {
	lexer       lexer.Lexer
	vm          vm.VM
	compiler    compiler.Compiler
	typechecker typechecker.TypeChecker
	importer    lexer.Importer
	debugger    debugger.Debugger
	args        []string
}

func show_err_and_exit(err error) {
	fmt.Fprint(os.Stderr, err.Error())
	utils.Exit(1)
}

func NewInterpreter(arguments []string, pkg_dir string, s *vm.VmSettings) *Interpreter {
	i := &Interpreter{
		lexer:       lexer.Lexer{},
		vm:          *vm.NewVM(s, compiler.GlobalScopeName),
		compiler:    *compiler.NewCompiler(s.Debug),
		typechecker: *typechecker.NewTypeChecker(s.TypeCheck),
		args:        arguments,
		importer:    *lexer.NewImporter(pkg_dir, s.IncludePaths),
	}

	i.importer.Paths = append(i.importer.Paths, s.IncludePaths...)
	return i
}

func (i *Interpreter) Prepare(fn string) {
	tokens, err := i.lexer.ProcessFile(fn, []string{}, &i.importer)
	if err != nil {
		show_err_and_exit(err)
	}

	if err = i.compiler.CompileTokens(tokens, &i.vm.Rc.Settings); err != nil {
		show_err_and_exit(err)
	}

	if err = i.typechecker.TypeCheckProgram(&i.compiler.Ops, &i.compiler.Ctx); err != nil {
		show_err_and_exit(err)
	}
}

func (i *Interpreter) Run(fn string) vm.ExitCodeType {
	i.Prepare(fn)
	return i.vm.Interprete(&i.compiler.Ops, i.args)
}

func (i *Interpreter) RunDebug(fn string) {
	i.Prepare(fn)
	i.vm.Rc.PrepareMemory(i.args, &i.vm.S)
	i.debugger = *debugger.NewDebugger(&i.vm)

	for {
		i.vm.Rc.Reset()
		go i.debugger.Debug(&i.compiler.Ops, i.args, &i.compiler.Ctx)
		res := i.debugger.Run()
		if res == debugger.DebugQuit {
			break
		}
	}
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
