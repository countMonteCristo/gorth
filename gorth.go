package main

import (
	"Gorth/interpreter"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"
	"flag"
	"path/filepath"
	"runtime"
)

var (
	_, main_filepath, _, _ = runtime.Caller(0)
	package_dir            = filepath.Dir(main_filepath)
)

const (
	vm_memory_size  = 640 * 1024 // 640K is enough for everybody :)
	recursion_limit = 1000
)

func main() {

	var includePaths utils.ArrayArgs

	debugFlag := flag.Bool("debug", false, "run with debuger mode")
	envFlag := flag.Bool("env", false, "save environment variables to VM memory")
	tcFlag := flag.Bool("disable-typecheck", false, "disable type checking")
	optFlag := flag.Int("O", 0, "optimization level")
	flag.Var(&includePaths, "I", "provide additional include paths")

	flag.Parse()

	settings := vm.NewSettings(
		*debugFlag, *envFlag, !*tcFlag, vm_memory_size, recursion_limit, includePaths,
		*optFlag,
	)

	gorth_script := flag.Args()[0]
	i := interpreter.NewInterpreter(flag.Args(), package_dir, settings)

	if !*debugFlag {
		exit_code := i.Run(gorth_script)
		i.ProcessExit(exit_code)
	} else {
		i.RunDebug(gorth_script)
	}
}
