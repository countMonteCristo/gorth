package main

import (
	"Gorth/interpreter"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/settings"
	"Gorth/interpreter/utils"
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

	modeFlag := flag.String("mode", "interprete", "how to run (interprete|debug|profile|dump)")
	logLevel := flag.String("log", "info", "log level")
	envFlag := flag.Bool("env", false, "save environment variables to VM memory")
	tcFlag := flag.Bool("disable-typecheck", false, "disable type checking")
	optFlag := flag.Int("O", 0, "optimization level")
	dumpFlag := flag.Bool("p", false, "only compile script, do not execute")
	flag.Var(&includePaths, "I", "provide additional include paths")

	flag.Parse()

	gorth_script := flag.Args()[0]

	settings := settings.NewSettings(
		*modeFlag, *envFlag, !*tcFlag, vm_memory_size, recursion_limit, includePaths,
		*optFlag, gorth_script+".prof", *logLevel, *dumpFlag,
	)

	i := interpreter.NewInterpreter(flag.Args(), package_dir, settings)

	switch *modeFlag {
	case "interprete":
		exit_code := i.Run(gorth_script)
		i.ProcessExit(exit_code)
	case "debug":
		i.RunDebug(gorth_script)
	case "profile":
		if *dumpFlag {
			logger.Crash("Can not use dump in `profile` mode")
		}
		i.RunProfile(gorth_script)
	default:
		logger.Crash("Unknown mode: %s", *modeFlag)
	}
}
