package interpreter

import (
	"Gorth/interpreter/compiler"
	"Gorth/interpreter/debugger"
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/optimizer"
	"Gorth/interpreter/profiler"
	"Gorth/interpreter/settings"
	"Gorth/interpreter/typechecker"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"
	"fmt"
	"os"
)

// ---------------------------------------------------------------------------------------------------------------------

type Interpreter struct {
	lexer       lexer.Lexer
	vm          vm.VM
	compiler    compiler.Compiler
	typechecker typechecker.TypeChecker
	optimizer   optimizer.Optimizer
	importer    lexer.Importer
	debugger    debugger.Debugger
	profiler    profiler.Profiler
	args        []string
	s           settings.Settings
}

func NewInterpreter(arguments []string, pkg_dir string, s *settings.Settings) *Interpreter {
	i := &Interpreter{
		lexer:       lexer.Lexer{},
		vm:          *vm.NewVM(s, compiler.GlobalScopeName),
		compiler:    *compiler.NewCompiler(s.IsDebug()),
		typechecker: *typechecker.NewTypeChecker(s.TypeCheck),
		optimizer:   *optimizer.NewOptimizer(s.Optimization),
		args:        arguments,
		importer:    *lexer.NewImporter(pkg_dir, s.IncludePaths),
		s:           *s,
	}

	i.importer.Paths = append(i.importer.Paths, s.IncludePaths...)
	return i
}

// ---------------------------------------------------------------------------------------------------------------------

func (i *Interpreter) Prepare(fn string) {
	token_holder, err := i.lexer.ProcessFile(fn, []string{}, &i.importer, &i.s)
	if err != nil {
		utils.ExitWithError(err)
	}

	if err = i.compiler.CompileTokens(token_holder, &i.vm.Rc.Settings, &i.s); err != nil {
		utils.ExitWithError(err)
	}

	if err = i.typechecker.TypeCheckProgram(&i.compiler.Ops, &i.compiler.Ctx, &i.s); err != nil {
		utils.ExitWithError(err)
	}

	if err = i.optimizer.Optimize(&i.compiler.Ops, &i.compiler.Ctx, &i.vm.Rc.Settings, &i.s); err != nil {
		utils.ExitWithError(err)
	}
}

// ---------------------------------------------------------------------------------------------------------------------

func (i *Interpreter) Run(fn string) vm.ExitCodeType {
	i.Prepare(fn)
	if i.s.Dump {
		i.vm.DumpOps(fn+"c", &i.compiler.Ops, &i.s)
		return vm.ExitCodeType{Code: 0, Msg: ""}
	} else {
		return i.vm.Interprete(&i.compiler.Ops, i.args, &i.s)
	}

}

func (i *Interpreter) RunDebug(fn string) {
	i.Prepare(fn)
	i.vm.Rc.PrepareMemory(i.args, &i.s)

	if i.s.Dump {
		i.vm.DumpOps(fn+"c", &i.compiler.Ops, &i.s)
	} else {
		i.debugger = *debugger.NewDebugger(&i.vm, &i.s)

		for {
			i.vm.Rc.Reset()
			go i.debugger.Debug(&i.compiler.Ops, i.args, &i.compiler.Ctx)
			res := i.debugger.Run()
			if res == debugger.DebugQuit {
				break
			}
		}
	}
}

func (i *Interpreter) RunProfile(fn string) {
	i.Prepare(fn)
	i.vm.PrepareRuntimeContext(i.args, &i.s)
	i.profiler = *profiler.NewProfiler(&i.vm, i.s.ProfilePath, &i.s)
	i.profiler.Run(&i.compiler.Ops, i.args)
}

// ---------------------------------------------------------------------------------------------------------------------

func (i *Interpreter) ProcessExit(exit_code vm.ExitCodeType) {
	if exit_code.Code != 0 {
		fmt.Fprintf(os.Stderr, "Script finished with exit code %d", exit_code.Code)
		if len(exit_code.Msg) > 0 {
			fmt.Fprintf(os.Stderr, ":\n%s", exit_code.Msg)
		}
		fmt.Fprintln(os.Stderr)
	}
	os.Exit(int(exit_code.Code))
}

// ---------------------------------------------------------------------------------------------------------------------
