package main

import (
	"Gorth/interpreter"
	"Gorth/interpreter/vm"
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
		debugger_interface := vm.NewDebugInterface()
		i.RunDebug(gorth_script, debugger_interface)

		for {
			input := ""
			fmt.Print("> ")
			fmt.Scanln(&input)
			input = strings.ToLower(input)

			if input == "h" || input == "help" {
				fmt.Println("Availavle commands:")
				fmt.Println(" * n -  step (process singe instruction)")
				fmt.Println(" * c -  continue (process all instructions until the end)")
				fmt.Println(" * t -  print current token")
				fmt.Println(" * o -  print current operation")
				fmt.Println(" * ol - print operations list")
				fmt.Println(" * s -  print current stack state")
				fmt.Println(" * m -  print current memory state")
				fmt.Println(" * e -  print current local and global environment (consts, allocs)")
				fmt.Println(" * h -  print help")
				fmt.Println(" * q -  exit debugger")
				continue
			}

			cmd, ok := vm.ParseDebuggerCommand(input)
			if !ok {
				fmt.Printf("Unknown command: <%s>\n", input)
				continue
			}

			response := debugger_interface.Communicate(cmd)
			if response.Status == vm.DebugCommandStatusFailed {
				fmt.Printf("[FAILED] %s\n", response.Msg)
			}

			if cmd.Type == vm.DebugCmdQuit {
				break
			}
		}
	}

}
