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
	tcFlag := flag.Bool("check", false, "enable type checking")
	flag.Var(&includePaths, "I", "provide additional include paths")

	flag.Parse()

	settings := vm.NewSettings(*debugFlag, *envFlag, *tcFlag, vm_memory_size, recursion_limit, includePaths)

	gorth_script := flag.Args()[0]
	i := interpreter.NewInterpreter(flag.Args(), package_dir, settings)

	if !*debugFlag {
		exit_code := i.Run(gorth_script)
		i.ProcessExit(exit_code)
	} else {
		i.RunDebug(gorth_script)
	}

	// for {
	// 	once := true

	// 	i := interpreter.NewInterpreter(flag.Args(), package_dir, settings)
	// 	// debugger_interface := debugger.NewDebugInterface()
	// 	i.RunDebug(gorth_script)

	// 	scanner := bufio.NewScanner(os.Stdin)
	// 	for {
	// 		fmt.Print("> ")
	// 		scanner.Scan()
	// 		input := scanner.Text()

	// 		cmd, ok := debugger.ParseDebuggerCommand(input)
	// 		if !ok {
	// 			fmt.Printf("Bad command: <%s>\n", input)
	// 			continue
	// 		}

	// 		response := debugger_interface.Communicate(cmd)
	// 		if response.Status == debugger.DebugCommandStatusFailed {
	// 			fmt.Printf("[FAILED] %s\n", response.Msg)
	// 		}

	// 		if cmd.Type == debugger.DebugCmdQuit {
	// 			break
	// 		}

	// 		if cmd.Type == debugger.DebugCmdRestart {
	// 			fmt.Printf("[INFO] Restart script\n")
	// 			once = false
	// 			break
	// 		}
	// 	}

	// 	if once {
	// 		break
	// 	}
	// }

}
