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

		scanner := bufio.NewScanner(os.Stdin)
		for {
			fmt.Print("> ")
			scanner.Scan()
			input := strings.ToLower(scanner.Text())

			if input == "h" || input == "help" {
				fmt.Println("Availavle commands:")
				fmt.Println(" * `n` [`count`]       - process at most `count` instructions (by default `count`=1)")
				fmt.Println(" * `c`                 - continue (process all instructions to the break point or to the end)")
				fmt.Println(" * `bs` `a1 a2 .. ak`  - set break points for functions or addresses")
				fmt.Println(" * `bl`                - list all break points")
				fmt.Println(" * `br` `a1 a2 .. ak`  - remove break points from functions or addresses")
				fmt.Println(" * `t`                 - print current token")
				fmt.Println(" * `o` [`ctx`]         - print current operation (+-`ctx` operations, by default `ctx`=0)")
				fmt.Println(" * `ol`                - print operations list")
				fmt.Println(" * `s`                 - print current stack state")
				fmt.Println(" * `m`                 - print current memory state")
				fmt.Println(" * `mo` `addr` `size`  - print memory chunk of size `size` at address `addr`")
				fmt.Println(" * `e`                 - print current local and global environment (consts, allocs)")
				fmt.Println(" * `h`                 - print help")
				fmt.Println(" * `q`                 - exit debugger")
				continue
			}

			cmd, ok := vm.ParseDebuggerCommand(input)
			if !ok {
				fmt.Printf("Wrong command: <%s>\n", input)
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
