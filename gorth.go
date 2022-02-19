package main

import (
	"GoStudy/Gorth/interpreter"
	"flag"
)

func main() {
	debugFlag := flag.Bool("debug", false, "run in debug mode")
	flag.Parse()

	gorth_script := flag.Args()[0]

	i := interpreter.InitInterpreter(flag.Args())
	i.Run(gorth_script, *debugFlag)
}
