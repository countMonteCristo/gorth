package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"fmt"
	"sort"
	"unsafe"

	"golang.org/x/sys/unix"
)

type VM struct {
	Ctx            CompileTimeContext
	RecursionLimit int
	MemorySize     types.IntType
	Rc             RunTimeContext
}

func InitVM() *VM {
	vm := VM{
		Ctx:            *InitContext(),
		RecursionLimit: 1000,
		MemorySize:     640 * 1024, // 640k is enough for everybody, huh?,
	}
	vm.Rc = *NewRuntimeContext(vm.MemorySize)

	return &vm
}

func (vm *VM) PreprocessTokens(th *lexer.TokenHolder) {
	vm.Rc.PreprocessStringLiterals(th, &vm.Ctx.StringsMap)
}

func (rc *RunTimeContext) PreprocessStringLiterals(th *lexer.TokenHolder, strings *map[string]types.IntType) {
	address := types.IntType(1)

	th.Reset()
	for !th.Empty() {
		token := th.GetNextToken()
		if token.Typ == lexer.TokenString {
			literal := token.Value.(string)
			_, exists := (*strings)[literal]
			if !exists {
				(*strings)[literal] = address
				address += types.IntType(len(literal) + 1) // save literals as null-terminated strings
			}
		}
	}

	rc.Memory.StringsRegion = MemoryRegion{
		Start: 1,
		Size:  address - 1,
		Ptr:   address,
	}
}

func (vm *VM) ProcessSyscall() {
	syscall_id := vm.Rc.Stack.Pop().(types.IntType)
	switch syscall_id {
	case unix.SYS_OPEN:
		mode := vm.Rc.Stack.Pop().(types.IntType)
		flags := vm.Rc.Stack.Pop().(types.IntType)
		ptr := vm.Rc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_OPEN, uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[ptr])), uintptr(flags), uintptr(mode),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_READ:
		count := vm.Rc.Stack.Pop().(types.IntType)
		ptr := vm.Rc.Stack.Pop().(types.IntType)
		fd := vm.Rc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_READ, uintptr(fd), uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[ptr])), uintptr(count),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_CLOSE:
		fd := vm.Rc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_CLOSE, uintptr(fd), 0, 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_WRITE:
		count := vm.Rc.Stack.Pop().(types.IntType)
		ptr := vm.Rc.Stack.Pop().(types.IntType)
		fd := vm.Rc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_WRITE, uintptr(fd), uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[ptr])), uintptr(count),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	default:
		panic(fmt.Sprintf("Syscall #%d is not implemented yet\n", syscall_id))
	}
}

func (vm *VM) Step(ops []Op) (err error) {
	op := ops[vm.Rc.Addr]
	switch op.Typ {
	case OpPushInt, OpPushBool:
		vm.Rc.Stack.Push(op.Operand)
		vm.Rc.Addr++
	case OpIf, OpDo:
		top := vm.Rc.Stack.Pop().(types.BoolType)
		if I2B(top) {
			vm.Rc.Addr++
		} else {
			vm.Rc.Addr += op.Operand.(types.IntType)
		}
	case OpElse, OpEnd, OpBreak, OpContinue:
		vm.Rc.Addr += op.Operand.(types.IntType)
	case OpWhile:
		vm.Rc.Addr++
	case OpIntrinsic:
		intrinsic := op.Operand.(lexer.IntrinsicType)
		switch intrinsic {
		case lexer.IntrinsicPlus, lexer.IntrinsicMinus, lexer.IntrinsicMul, lexer.IntrinsicBitAnd, lexer.IntrinsicBitOr, lexer.IntrinsicBitXor:
			b := vm.Rc.Stack.Pop().(types.IntType)
			a := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Stack.Push(SafeArithmeticFunctions[intrinsic](a, b))
		case lexer.IntrinsicDiv:
			b := vm.Rc.Stack.Pop().(types.IntType)
			if b == 0 {
				return logger.FormatRuntimeErrMsg(&op.OpToken.Loc, "Division by zero")
			}
			a := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Stack.Push(a / b)
		case lexer.IntrinsicMod:
			b := vm.Rc.Stack.Pop().(types.IntType)
			if b == 0 {
				return logger.FormatRuntimeErrMsg(&op.OpToken.Loc, "Division by zero")
			}
			a := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Stack.Push(a % b)
		case lexer.IntrinsicShl:
			b := vm.Rc.Stack.Pop().(types.IntType)
			if b < 0 {
				return logger.FormatRuntimeErrMsg(&op.OpToken.Loc, "Negative shift amount in `<<`: %d", b)
			}
			a := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Stack.Push(a << b)
		case lexer.IntrinsicShr:
			b := vm.Rc.Stack.Pop().(types.IntType)
			if b < 0 {
				return logger.FormatRuntimeErrMsg(&op.OpToken.Loc, "Negative shift amount in `>>`: %d", b)
			}
			a := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Stack.Push(a >> b)
		case lexer.IntrinsicBitNot:
			a := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Stack.Push(^a)
		case lexer.IntrinsicLogicalAnd, lexer.IntrinsicLogicalOr:
			b := vm.Rc.Stack.Pop().(types.BoolType)
			a := vm.Rc.Stack.Pop().(types.BoolType)
			vm.Rc.Stack.Push(LogicalFunctions[intrinsic](a, b))
		case lexer.IntrinsicLogicalNot:
			x := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(B2I(!I2B(x.(types.BoolType))))
		case lexer.IntrinsicDup:
			x := vm.Rc.Stack.Top()
			vm.Rc.Stack.Push(x)
		case lexer.IntrinsicSwap:
			b := vm.Rc.Stack.Pop()
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(b)
			vm.Rc.Stack.Push(a)
		case lexer.IntrinsicDrop:
			vm.Rc.Stack.Pop()
		case lexer.IntrinsicOver:
			x := vm.Rc.Stack.Data[len(vm.Rc.Stack.Data)-2]
			vm.Rc.Stack.Push(x)
		case lexer.IntrinsicRot:
			c := vm.Rc.Stack.Pop()
			b := vm.Rc.Stack.Pop()
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(b)
			vm.Rc.Stack.Push(c)
			vm.Rc.Stack.Push(a)
		case lexer.IntrinsicEq, lexer.IntrinsicNe, lexer.IntrinsicLe, lexer.IntrinsicGe, lexer.IntrinsicLt, lexer.IntrinsicGt:
			b := vm.Rc.Stack.Pop().(types.IntType)
			a := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Stack.Push(ComparableFunctions[intrinsic](a, b))
		case lexer.IntrinsicPuti:
			x := vm.Rc.Stack.Pop()
			fmt.Print(x)
		case lexer.IntrinsicDebug:
			fmt.Printf(
				"\tMem: %v\tStack: %v\n",
				vm.Rc.Memory.Data[vm.Rc.Memory.OperativeMemRegion.Start:vm.Rc.Memory.OperativeMemRegion.Ptr],
				vm.Rc.Stack.Data,
			)
		case lexer.IntrinsicLoad8, lexer.IntrinsicLoad16, lexer.IntrinsicLoad32, lexer.IntrinsicLoad64:
			ptr := vm.Rc.Stack.Pop().(types.IntType)
			val := vm.Rc.Memory.LoadFromMem(ptr, LoadSizes[intrinsic])
			vm.Rc.Stack.Push(val)
		case lexer.IntrinsicStore8, lexer.IntrinsicStore16, lexer.IntrinsicStore32, lexer.IntrinsicStore64:
			ptr := vm.Rc.Stack.Pop().(types.IntType)
			x := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Memory.StoreToMem(ptr, x, StoreSizes[intrinsic])

		case lexer.IntrinsicArgc:
			vm.Rc.Stack.Push(types.IntType(len(vm.Rc.Args)))
		case lexer.IntrinsicArgv:
			vm.Rc.Stack.Push(vm.Rc.Memory.Argv)
		case lexer.IntrinsicSyscall:
			vm.ProcessSyscall()
		default:
			return logger.FormatRuntimeErrMsg(&op.OpToken.Loc, "Unhandled intrinsic: `%s`", op.OpToken.Text)
		}
		vm.Rc.Addr++
	case OpCall:
		if vm.Rc.ReturnStack.Size() >= vm.RecursionLimit {
			return logger.FormatRuntimeErrMsg(&op.OpToken.Loc, "Recursion limit exceeded")
		}
		vm.Rc.ReturnStack.Push(vm.Rc.Addr)
		vm.Rc.Addr += op.Operand.(types.IntType)
	case OpFuncBegin:
		vm.Rc.Memory.OperativeMemRegion.Ptr += op.Operand.(types.IntType)
		vm.Rc.Addr++
		if vm.Rc.debug {
			vm.Rc.ScopeStack.Push(op.DebugInfo.(string))
		}
	case OpFuncEnd:
		if vm.Rc.ReturnStack.Size() == 0 {
			return logger.FormatRuntimeErrMsg(&op.OpToken.Loc, "Return stack is empty")
		}
		vm.Rc.Addr = vm.Rc.ReturnStack.Pop().(types.IntType) + 1
		vm.Rc.Memory.OperativeMemRegion.Ptr -= op.Operand.(types.IntType)
		if vm.Rc.debug {
			vm.Rc.ScopeStack.Pop()
		}
	case OpPushLocalAlloc:
		addr := vm.Rc.Memory.OperativeMemRegion.Ptr - op.Operand.(types.IntType)
		vm.Rc.Stack.Push(addr)
		vm.Rc.Addr++
	case OpPushGlobalAlloc:
		addr := vm.Rc.Memory.OperativeMemRegion.Start + op.Operand.(types.IntType)
		vm.Rc.Stack.Push(addr)
		vm.Rc.Addr++
	default:
		return logger.FormatRuntimeErrMsg(&op.OpToken.Loc, "Unhandled operation: `%s`", OpName[op.Typ])
	}
	return
}

func (vm *VM) PrepareRuntimeContext(ops []Op, args []string, debug bool) {
	len_ops := types.IntType(len(ops))

	vm.Rc.OpsCount = len_ops
	vm.Rc.ReturnStack.Push(len_ops)

	vm.Rc.Args = args
	vm.Rc.Memory.Prepare(args, &vm.Ctx.StringsMap)

	vm.Rc.debug = debug
	vm.Rc.Addr = vm.Ctx.Funcs["main"].Addr

	vm.Rc.Memory.OperativeMemRegion.Ptr = vm.Rc.Memory.OperativeMemRegion.Start + vm.Ctx.GlobalScope().MemSize
}

func (vm *VM) Interprete(ops []Op, args []string) ExitCodeType {

	// rc := vm.Prepare(ops, args, false)
	vm.PrepareRuntimeContext(ops, args, false)
	var err error = nil
	for vm.Rc.Addr < vm.Rc.OpsCount {
		err = vm.Step(ops)
		if err != nil {
			break
		}
	}
	return vm.Rc.GetExitCode(ops, err)
}

func (vm *VM) InterpreteDebug(ops []Op, args []string, di *DebugInterface) {
	vm.PrepareRuntimeContext(ops, args, true)

loop:
	for {
		cmd := <-di.Commands

		switch cmd.Type {
		case DebugCmdStack: // print stack
			fmt.Println(vm.Rc.Stack.Data)
			di.SendOK()
		case DebugCmdMemory: // print memory
			vm.Rc.Memory.PrintDebug()
			di.SendOK()
		case DebugCmdOperativeMemory:
			chunk := cmd.Args.([]int)
			start, size := chunk[0], chunk[1]
			if start >= int(vm.Rc.Memory.MemorySize) {
				di.SendFailed(fmt.Sprintf("Start address %d is out of bounds", start))
				continue
			}
			end := start + size
			if end >= int(vm.Rc.Memory.MemorySize) {
				end = int(vm.Rc.Memory.MemorySize) - 1
			}
			memory_chunk := vm.Rc.Memory.Data[start:end]
			fmt.Printf("addr=%d size=%d: %v\n", start, size, memory_chunk)
			di.SendOK()
		case DebugCmdPrint:
			names := cmd.Args.([]string)
			scope_name := vm.Rc.ScopeStack.Top().(string)
			n_found := vm.Ctx.DebugConstNames(names, scope_name) +
				vm.Ctx.DebugAllocNames(names, scope_name, &vm.Rc.Memory) +
				vm.Ctx.DebugFuncNames(names)
			if n_found > 0 {
				di.SendOK()
			} else {
				di.SendFailed(fmt.Sprintf("Failed to print values for names: %v", names))
			}
		case DebugCmdQuit, DebugCmdRestart: // quit or restart
			di.SendOK()
			break loop
		case DebugCmdStep: // step
			if vm.Rc.Addr >= vm.Rc.OpsCount {
				di.SendFailed("Can not step: script finished")
			} else {
				steps_count := cmd.Args.(int)
				var err error = nil
				for i := 0; i < steps_count && vm.Rc.Addr < vm.Rc.OpsCount; i++ {
					err = vm.Step(ops)
					if err != nil {
						break
					}

					addr, is_bp := di.IsBreakpoint(&vm.Rc, ops)
					if is_bp {
						fmt.Printf("[INFO] Break at address %d\n", addr)
						break
					}
				}

				if err != nil {
					di.SendFailed(fmt.Sprintf("Script failed because of: %s", err.Error()))
				} else {
					if vm.Rc.Addr >= vm.Rc.OpsCount {
						ec := vm.Rc.GetExitCode(ops, err)
						fmt.Printf("[INFO] Script finished with exit code %d", ec.Code)
						if len(ec.Msg) > 0 {
							fmt.Printf(": %s", ec.Msg)
						}
						fmt.Println()
					}
					di.SendOK()
				}
			}
		case DebugCmdContinue: // continue
			if vm.Rc.Addr >= vm.Rc.OpsCount {
				di.SendFailed("Can not continue: script finished")
			} else {
				var err error = nil
				for vm.Rc.Addr < vm.Rc.OpsCount {
					err = vm.Step(ops)
					if err != nil {
						break
					}

					addr, is_bp := di.IsBreakpoint(&vm.Rc, ops)
					if is_bp {
						fmt.Printf("[INFO] Break at address %d\n", addr)
						break
					}
				}

				if err != nil {
					di.SendFailed(fmt.Sprintf("Script failed because of: %s", err.Error()))
				} else {
					if vm.Rc.Addr >= vm.Rc.OpsCount {
						ec := vm.Rc.GetExitCode(ops, err)
						fmt.Printf("[INFO] Script finished with exit code %d", ec.Code)
						if len(ec.Msg) > 0 {
							fmt.Printf(": %s", ec.Msg)
						}
						fmt.Println()
					}
					di.SendOK()
				}
			}
		case DebugCmdBreakpointSet:
			bps := cmd.Args.(BreakPointList)

			found_names := make([]string, 0, len(bps.Funcs))
			for _, func_name := range bps.Funcs {
				function, exists := vm.Ctx.Funcs[func_name]
				if !exists {
					fmt.Printf("[WARN] Can not set break point to unknown function `%s`, skip\n", func_name)
				} else {
					found_names = append(found_names, func_name)
					di.BreakPoints[function.Addr] = true
				}
			}

			found_addr := make([]types.IntType, 0, len(bps.Addr))
			for _, addr := range bps.Addr {
				if addr >= vm.Rc.OpsCount || addr < 0 {
					fmt.Printf("[WARN] Address %d is out of bounds. skip\n", addr)
				} else {
					di.BreakPoints[addr] = true
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
				di.SendOK()
			} else {
				di.SendFailed(
					fmt.Sprintf("Can not set break points for functions=%v and addresses=%v", bps.Funcs, bps.Addr),
				)
			}
		case DebugCmdBreakpointList:
			addresses := make([]types.IntType, 0, len(di.BreakPoints))
			for addr := range di.BreakPoints {
				addresses = append(addresses, addr)
			}
			sort.Slice(addresses, func(i, j int) bool { return addresses[i] < addresses[j] })
			for _, addr := range addresses {
				fmt.Printf("b%s\n", ops[addr].Str(addr))
			}
			if len(addresses) == 0 {
				fmt.Println("[INFO] No breakpoints were set")
			}
			di.SendOK()
		case DebugCmdBreakpointRemove:
			bps := cmd.Args.(BreakPointList)

			removed_names := make([]string, 0, len(bps.Funcs))
			for _, func_name := range bps.Funcs {
				function, exists := vm.Ctx.Funcs[func_name]
				if !exists {
					fmt.Printf("[WARN] Can not remove break point from unknown function `%s`\n", func_name)
				} else {
					_, exists := di.BreakPoints[function.Addr]
					if !exists {
						fmt.Printf("[WARN] Can not remove break point from function `%s` - it was not set\n", func_name)
					} else {
						removed_names = append(removed_names, func_name)
						delete(di.BreakPoints, function.Addr)
					}
				}
			}

			removed_addr := make([]types.IntType, 0, len(bps.Addr))
			for _, addr := range bps.Addr {
				_, exists := di.BreakPoints[addr]
				if !exists {
					fmt.Printf("[WARN] Can not remove break point from address `%d` - it was not set, skip\n", addr)
				} else {
					removed_addr = append(removed_addr, addr)
					delete(di.BreakPoints, addr)
				}
			}

			if len(removed_names) > 0 {
				fmt.Printf("[INFO] Remove break points from functions %v\n", removed_names)
			}
			if len(removed_addr) > 0 {
				fmt.Printf("[INFO] Remove break points from addresses %v\n", removed_addr)
			}

			if len(removed_names)+len(removed_addr) > 0 {
				di.SendOK()
			} else {
				di.SendFailed(
					fmt.Sprintf("Can not remove break points for functions=%v and addresses=%v", bps.Funcs, bps.Addr),
				)
			}
		case DebugCmdToken: // token
			if vm.Rc.Addr >= vm.Rc.OpsCount {
				di.SendFailed("Can not print token: script finished")
			} else {
				token := ops[vm.Rc.Addr].OpToken
				fmt.Printf("%s:%d:%d Token(%s)\n", token.Loc.Filepath, token.Loc.Line+1, token.Loc.Column+1, token.Text)
				di.SendOK()
			}
		case DebugCmdOperation: // operation
			if vm.Rc.Addr >= vm.Rc.OpsCount {
				di.SendFailed("Can not print operation: script finished")
			} else {
				context_size := cmd.Args.(types.IntType)
				di.PrintOpsList(vm.Rc.Addr-context_size, vm.Rc.Addr+context_size, ops, &vm.Rc)
				di.SendOK()
			}
		case DebugCmdOperationList: // print ops list
			di.PrintOpsList(0, vm.Rc.OpsCount-1, ops, &vm.Rc)
			di.SendOK()
		case DebugCmdEnv: // env - consts and allocs
			typ := cmd.Args.(string)
			if vm.Rc.Addr >= vm.Rc.OpsCount {
				di.SendFailed("Can not print environment: script finished")
			} else {
				current_scope_name := vm.Rc.ScopeStack.Top().(string)

				if typ == "all" || typ == "local" {
					fmt.Printf("Scope: <%s>\n", current_scope_name)
					if current_scope_name != GlobalScopeName {
						fmt.Printf("Locals: ")
						vm.Ctx.DebugConsts(current_scope_name)
						vm.Ctx.DebugAllocs(current_scope_name, &vm.Rc.Memory)
						fmt.Println()
					}
				}

				if typ == "all" || typ == "global" {
					fmt.Printf("Globals: ")
					vm.Ctx.DebugConsts(GlobalScopeName)
					vm.Ctx.DebugAllocs(GlobalScopeName, &vm.Rc.Memory)
					fmt.Println()
				}

				di.SendOK()
			}
		case DebugCmdHelp:
			di.SendOK()
		default:
			di.SendFailed(fmt.Sprintf("Unknown command: '%s'", cmd.Name))
		}
	}
}
