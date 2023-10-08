package main

import (
	"Gorth/interpreter"
	"Gorth/interpreter/utils"
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

const (
	vm_memory_size  = 640 * 1024 // 640K is enough for everybody :)
	recursion_limit = 1000
)

func main() {

	var includePaths utils.ArrayArgs

	debugFlag := flag.Bool("debug", false, "run with debuger mode")
	envFlag := flag.Bool("env", false, "save environment variables to VM memory")
	flag.Var(&includePaths, "I", "provide additional include paths")

	flag.Parse()

	settings := vm.NewSettings(*debugFlag, *envFlag, vm_memory_size, recursion_limit, includePaths)

	gorth_script := flag.Args()[0]

	if !*debugFlag {
		i := interpreter.InitInterpreter(flag.Args(), package_dir, settings)
		exit_code := i.Run(gorth_script)
		i.ProcessExit(exit_code)
	}

	for {
		once := true

		i := interpreter.InitInterpreter(flag.Args(), package_dir, settings)
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
