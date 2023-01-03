package main

import (
	"Gorth/interpreter"
	"flag"
	"fmt"
	"path/filepath"
	"runtime"
	"strings"
)

var (
	_, main_filepath, _, _ = runtime.Caller(0)
	package_dir            = filepath.Dir(main_filepath)
)

func main() {
	debugFlag := flag.Bool("debug", false, "run with debuger mode")
	flag.Parse()

	gorth_script := flag.Args()[0]

	i := interpreter.InitInterpreter(flag.Args(), package_dir)

	if !*debugFlag {
		i.Run(gorth_script)
	} else {
		cmd_chan := make(chan string)
		resp_chan := make(chan string)
		i.RunDebug(gorth_script, cmd_chan, resp_chan)

		for {
			input := ""
			fmt.Print("> ")
			fmt.Scanln(&input)
			input = strings.ToLower(input)

			if input == "h" || input == "help" {
				fmt.Println("Availavle commands:")
				fmt.Println(" * n - step (process singe instruction)")
				fmt.Println(" * t - print current token")
				fmt.Println(" * o - print current operation")
				fmt.Println(" * s - print current stack state")
				fmt.Println(" * m - print current memory state")
				fmt.Println(" * h - print help")
				fmt.Println(" * q - exit debugger")
				continue
			}

			cmd_chan <- input
			<-resp_chan

			if input == "q" {
				break
			}
		}
	}

}
