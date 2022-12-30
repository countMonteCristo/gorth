package main

import (
	"Gorth/interpreter"
	"flag"
	"path/filepath"
	"runtime"
)

var (
	_, main_filepath, _, _ = runtime.Caller(0)
	package_dir            = filepath.Dir(main_filepath)
)

func main() {
	debugFlag := flag.Bool("debug", false, "run in debug mode")
	flag.Parse()

	gorth_script := flag.Args()[0]

	i := interpreter.InitInterpreter(flag.Args(), package_dir)
	i.Run(gorth_script, *debugFlag)
}
