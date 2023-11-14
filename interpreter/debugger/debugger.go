package debugger

import (
	"Gorth/interpreter/compiler"
	"Gorth/interpreter/operations"
	"Gorth/interpreter/settings"
	"Gorth/interpreter/types"
	"Gorth/interpreter/vm"
	"bufio"
	"fmt"
	"os"
	"sort"
)

// ---------------------------------------------------------------------------------------------------------------------

type Debugger struct {
	vm    *vm.VM
	s     *settings.Settings
	iface DebugInterface
}

func NewDebugger(vm *vm.VM, s *settings.Settings) *Debugger {
	return &Debugger{
		vm:    vm,
		s:     s,
		iface: *NewDebugInterface(),
	}
}

// ---------------------------------------------------------------------------------------------------------------------

func (d *Debugger) Run() DebugTransition {
	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("> ")
		scanner.Scan()
		input := scanner.Text()

		cmd, ok := ParseDebuggerCommand(input)
		if !ok {
			fmt.Printf("Bad command: <%s>\n", input)
			continue
		}

		response := d.iface.Communicate(cmd)
		if response.Status == DebugCommandStatusFailed {
			fmt.Printf("[FAILED] %s\n", response.Msg)
		}

		if cmd.Type == DebugCmdQuit {
			return DebugQuit
		}

		if cmd.Type == DebugCmdRestart {
			fmt.Printf("[INFO] Restart script\n")
			return DebugRestart
		}
	}
}

// ---------------------------------------------------------------------------------------------------------------------

func (d *Debugger) Debug(ops *[]operations.Op, args []string, ctx *compiler.CompileTimeContext) {
loop:
	for {
		cmd := <-d.iface.Commands

		switch cmd.Type {
		case DebugCmdStack: // print stack
			fmt.Printf("stack: %v return_stack: %v\n", d.vm.Rc.Stack.Data, d.vm.Rc.ReturnStack)
			d.iface.SendOK()
		case DebugCmdMemory: // print memory
			d.vm.Rc.Memory.PrintDebug()
			d.iface.SendOK()
		case DebugCmdOperativeMemory:
			chunk := cmd.Args.([]int)
			start, size := chunk[0], chunk[1]
			if start >= d.vm.Rc.Memory.Size() {
				d.iface.SendFailed(fmt.Sprintf("Start address %d is out of bounds", start))
				continue
			}
			end := start + size
			if end >= d.vm.Rc.Memory.Size() {
				end = d.vm.Rc.Memory.Size() - 1
			}
			memory_chunk := d.vm.Rc.Memory.Data[start:end]
			fmt.Printf("addr=%d size=%d: %v\n", start, size, memory_chunk)
			d.iface.SendOK()
		case DebugCmdPrint:
			names := cmd.Args.([]string)
			scope_name := d.vm.Rc.Scopes.Top()
			n_found := ctx.DebugConstNames(names, scope_name) +
				ctx.DebugAllocNames(names, scope_name, &d.vm.Rc.Memory) +
				ctx.DebugFuncNames(names)
			if n_found > 0 {
				d.iface.SendOK()
			} else {
				d.iface.SendFailed(fmt.Sprintf("Failed to print values for names: %v", names))
			}
		case DebugCmdQuit, DebugCmdRestart: // quit or restart
			d.iface.SendOK()
			break loop
		case DebugCmdStep: // step
			if d.vm.Rc.Addr >= d.vm.Rc.OpsCount {
				d.iface.SendFailed("Can not step: script finished")
			} else {
				d.checkErr(d.doSteps(ops, cmd.Args.(int), false), ops)
			}
		case DebugCmdContinue: // continue
			if d.vm.Rc.Addr >= d.vm.Rc.OpsCount {
				d.iface.SendFailed("Can not continue: script finished")
			} else {
				d.checkErr(d.doSteps(ops, -1, false), ops)
			}
		case DebugCmdUp:
			if d.vm.Rc.Addr >= d.vm.Rc.OpsCount {
				d.iface.SendFailed("Can not go up: script finished")
			} else {
				d.checkErr(d.doSteps(ops, -1, true), ops)
			}
		case DebugCmdBreakpointSet:
			bps := cmd.Args.(BreakPointList)

			found_names := make([]string, 0, len(bps.Funcs))
			for _, func_name := range bps.Funcs {
				function, exists := ctx.Funcs[func_name]
				if !exists {
					fmt.Printf("[WARN] Can not set break point to unknown function `%s`, skip\n", func_name)
				} else {
					if function.Inlined {
						fmt.Printf("[WARN] Can not set break point to inline function `%s`, skip\n", func_name)
					} else {
						found_names = append(found_names, func_name)
						d.iface.BreakPoints[function.Addr] = true
					}
				}
			}

			found_addr := make([]types.IntType, 0, len(bps.Addr))
			for _, addr := range bps.Addr {
				if addr >= d.vm.Rc.OpsCount || addr < 0 {
					fmt.Printf("[WARN] Address %d is out of bounds. skip\n", addr)
				} else {
					d.iface.BreakPoints[addr] = true
					found_addr = append(found_addr, addr)
				}
			}

			if len(found_names) > 0 {
				fmt.Printf("[INFO] Set break point to functions %v\n", found_names)
			}
			if len(found_addr) > 0 {
				fmt.Printf("[INFO] Set break point for addresses %v\n", found_addr)
			}

			if len(found_names)+len(found_addr) > 0 {
				d.iface.SendOK()
			} else {
				d.iface.SendFailed(
					fmt.Sprintf("Can not set break points for functions=%v and addresses=%v", bps.Funcs, bps.Addr),
				)
			}
		case DebugCmdBreakpointList:
			addresses := make([]types.IntType, 0, len(d.iface.BreakPoints))
			for addr := range d.iface.BreakPoints {
				addresses = append(addresses, addr)
			}
			sort.Slice(addresses, func(i, j int) bool { return addresses[i] < addresses[j] })
			for _, addr := range addresses {
				fmt.Printf("b%s\n", (*ops)[addr].Str(addr))
			}
			if len(addresses) == 0 {
				fmt.Println("[INFO] No breakpoints were set")
			}
			d.iface.SendOK()
		case DebugCmdBreakpointRemove:
			bps := cmd.Args.(BreakPointList)

			removed_names := make([]string, 0, len(bps.Funcs))
			for _, func_name := range bps.Funcs {
				function, exists := ctx.Funcs[func_name]
				if !exists {
					fmt.Printf("[WARN] Can not remove break point from unknown function `%s`\n", func_name)
				} else {
					if function.Inlined {
						fmt.Printf("[WARN] Can not remove break point from inline function `%s`\n", func_name)
					} else {
						_, exists := d.iface.BreakPoints[function.Addr]
						if !exists {
							fmt.Printf("[WARN] Can not remove break point from function `%s` - it was not set\n", func_name)
						} else {
							removed_names = append(removed_names, func_name)
							delete(d.iface.BreakPoints, function.Addr)
						}
					}

				}
			}

			removed_addr := make([]types.IntType, 0, len(bps.Addr))
			for _, addr := range bps.Addr {
				_, exists := d.iface.BreakPoints[addr]
				if !exists {
					fmt.Printf("[WARN] Can not remove break point from address `%d` - it was not set, skip\n", addr)
				} else {
					removed_addr = append(removed_addr, addr)
					delete(d.iface.BreakPoints, addr)
				}
			}

			if len(removed_names) > 0 {
				fmt.Printf("[INFO] Remove break points from functions %v\n", removed_names)
			}
			if len(removed_addr) > 0 {
				fmt.Printf("[INFO] Remove break points from addresses %v\n", removed_addr)
			}

			if len(removed_names)+len(removed_addr) > 0 {
				d.iface.SendOK()
			} else {
				d.iface.SendFailed(
					fmt.Sprintf("Can not remove break points for functions=%v and addresses=%v", bps.Funcs, bps.Addr),
				)
			}
		case DebugCmdToken: // token
			if d.vm.Rc.Addr >= d.vm.Rc.OpsCount {
				d.iface.SendFailed("Can not print token: script finished")
			} else {
				token := (*ops)[d.vm.Rc.Addr].Token
				fmt.Printf("%s:%d:%d Token(%s)\n", token.Loc.Filepath, token.Loc.Line+1, token.Loc.Column+1, token.Text)
				d.iface.SendOK()
			}
		case DebugCmdOperation: // operation
			if d.vm.Rc.Addr >= d.vm.Rc.OpsCount {
				d.iface.SendFailed("Can not print operation: script finished")
			} else {
				context_size := cmd.Args.(types.IntType)
				d.iface.PrintOpsList(d.vm.Rc.Addr-context_size, d.vm.Rc.Addr+context_size, ops, &d.vm.Rc)
				d.iface.SendOK()
			}
		case DebugCmdOperationList: // print ops list
			d.iface.PrintOpsList(0, d.vm.Rc.OpsCount-1, ops, &d.vm.Rc)
			d.iface.SendOK()
		case DebugCmdEnv: // env - consts and allocs
			typ := cmd.Args.(string)
			if d.vm.Rc.Addr >= d.vm.Rc.OpsCount {
				d.iface.SendFailed("Can not print environment: script finished")
			} else {
				current_scope_name := d.vm.Rc.Scopes.Top()

				if typ == "all" || typ == "local" {
					fmt.Printf("Scope: <%s>\n", current_scope_name)
					if current_scope_name != compiler.GlobalScopeName {
						fmt.Printf("Locals: ")
						ctx.DebugConsts(current_scope_name)
						ctx.DebugAllocs(current_scope_name, &d.vm.Rc.Memory)
						fmt.Println()
					}
				}

				if typ == "all" || typ == "global" {
					fmt.Printf("Globals: ")
					ctx.DebugConsts(compiler.GlobalScopeName)
					ctx.DebugAllocs(compiler.GlobalScopeName, &d.vm.Rc.Memory)
					fmt.Println()
				}

				d.iface.SendOK()
			}
		case DebugCmdHelp:
			d.iface.SendOK()
		default:
			d.iface.SendFailed(fmt.Sprintf("Unknown command: '%s'", cmd.Name))
		}
	}
}

// ---------------------------------------------------------------------------------------------------------------------

func (d *Debugger) doSteps(ops *[]operations.Op, count int, go_up bool) error {
	i := 0
	for d.vm.Rc.Addr < d.vm.Rc.OpsCount {
		is_func_end := (*ops)[d.vm.Rc.Addr].Typ == operations.OpFuncEnd

		if err := d.vm.Step(ops, d.s); err != nil {
			return err
		}

		if addr, is_bp := d.iface.IsBreakpoint(&d.vm.Rc); is_bp {
			fmt.Printf("[INFO] Break at address %d\n", addr)
			return nil
		}

		if go_up && is_func_end {
			break
		}

		i++
		if i == count {
			break
		}
	}
	return nil
}

func (d *Debugger) checkErr(err error, ops *[]operations.Op) {
	if err != nil {
		d.iface.SendFailed(fmt.Sprintf("Script failed because of:\n%s", err.Error()))
	} else {
		if d.vm.Rc.Addr >= d.vm.Rc.OpsCount {
			ec := d.vm.Rc.GetExitCode(err)
			fmt.Printf("[INFO] Script finished with exit code %d", ec.Code)
			if len(ec.Msg) > 0 {
				fmt.Printf(":\n%s", ec.Msg)
			}
			fmt.Println()
		}
		d.iface.SendOK()
	}
}

// ---------------------------------------------------------------------------------------------------------------------
