package main

import (
	"Gorth/interpreter"
	"Gorth/interpreter/vm"
	"bufio"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
)

var (
	_, main_filepath, _, _ = runtime.Caller(0)
	package_dir            = filepath.Dir(main_filepath)
)

func main() {
	debugFlag := flag.Bool("debug", false, "run with debuger mode")
	flag.Parse()

	gorth_script := flag.Args()[0]

	if !*debugFlag {
		i := interpreter.InitInterpreter(flag.Args(), package_dir)
		exit_code := i.Run(gorth_script)
		i.ProcessExit(exit_code)
	}

	for {
		once := true

		i := interpreter.InitInterpreter(flag.Args(), package_dir)
		debugger_interface := vm.NewDebugInterface()
		i.RunDebug(gorth_script, debugger_interface)

		scanner := bufio.NewScanner(os.Stdin)
		for {
			fmt.Print("> ")
			scanner.Scan()
			input := scanner.Text()

			cmd, ok := vm.ParseDebuggerCommand(input)
			if !ok {
				fmt.Printf("Bad command: <%s>\n", input)
				continue
			}

			response := debugger_interface.Communicate(cmd)
			if response.Status == vm.DebugCommandStatusFailed {
				fmt.Printf("[FAILED] %s\n", response.Msg)
			}

			if cmd.Type == vm.DebugCmdQuit {
				break
			}

			if cmd.Type == vm.DebugCmdRestart {
				fmt.Printf("[INFO] Restart script\n")
				once = false
				break
			}
		}

		if once {
			break
		}
	}
}
