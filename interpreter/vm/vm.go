package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
	"sort"
	"unsafe"

	"golang.org/x/sys/unix"
)

type VM struct {
	Ctx            Context
	RecursionLimit int
}

func InitVM() *VM {
	vm := VM{
		Ctx:            *InitContext(640 * 1024), // 640k is enough for everybody, huh?
		RecursionLimit: 1000,
	}

	return &vm
}

func (vm *VM) PreprocessTokens(th *lexer.TokenHolder) {
	vm.Ctx.PreprocessStringLiterals(th)
}

type ExitCodeType struct {
	Code types.IntType
	Msg  string
}

type ScriptContext struct {
	Stack       utils.Stack
	ReturnStack utils.Stack
	ScopeStack  utils.Stack
	Addr        types.IntType
	Args        []string
	OpsCount    types.IntType
	ExitCode    ExitCodeType
	debug       bool
}

func NewScriptContext(len_ops types.IntType, args []string, debug bool) *ScriptContext {
	sc := &ScriptContext{
		Stack: utils.Stack{}, ReturnStack: utils.Stack{},
		Addr: 0, Args: args, OpsCount: len_ops, ScopeStack: utils.Stack{},
		ExitCode: ExitCodeType{Code: 0}, debug: debug,
	}
	sc.ReturnStack.Push(len_ops)
	sc.ScopeStack.Push(GlobalScopeName)
	return sc
}

func (sc *ScriptContext) GetExitCode(ops []Op) ExitCodeType {
	switch {
	case sc.Stack.Size() > 1:
		sc.ExitCode.Code = types.IntType(1)
		sc.ExitCode.Msg = fmt.Sprintf("Multiple values left in stack after script exit: %v", sc.Stack.Data)
	case sc.Stack.Size() == 0:
		sc.ExitCode.Code = types.IntType(2)
		sc.ExitCode.Msg = "Empty stack after script exit"
	default:
		sc.ExitCode.Code = sc.Stack.Pop().(types.IntType)
	}
	return sc.ExitCode
}

func (vm *VM) ProcessSyscall(sc *ScriptContext) {
	syscall_id := sc.Stack.Pop().(types.IntType)
	switch syscall_id {
	case unix.SYS_OPEN:
		mode := sc.Stack.Pop().(types.IntType)
		flags := sc.Stack.Pop().(types.IntType)
		ptr := sc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_OPEN, uintptr(unsafe.Pointer(&vm.Ctx.Memory.Data[ptr])), uintptr(flags), uintptr(mode),
		)
		sc.Stack.Push(types.IntType(r1))
		sc.Stack.Push(types.IntType(err))
	case unix.SYS_READ:
		count := sc.Stack.Pop().(types.IntType)
		ptr := sc.Stack.Pop().(types.IntType)
		fd := sc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_READ, uintptr(fd), uintptr(unsafe.Pointer(&vm.Ctx.Memory.Data[ptr])), uintptr(count),
		)
		sc.Stack.Push(types.IntType(r1))
		sc.Stack.Push(types.IntType(err))
	case unix.SYS_CLOSE:
		fd := sc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_CLOSE, uintptr(fd), 0, 0,
		)
		sc.Stack.Push(types.IntType(r1))
		sc.Stack.Push(types.IntType(err))
	case unix.SYS_WRITE:
		count := sc.Stack.Pop().(types.IntType)
		ptr := sc.Stack.Pop().(types.IntType)
		fd := sc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_WRITE, uintptr(fd), uintptr(unsafe.Pointer(&vm.Ctx.Memory.Data[ptr])), uintptr(count),
		)
		sc.Stack.Push(types.IntType(r1))
		sc.Stack.Push(types.IntType(err))
	default:
		panic(fmt.Sprintf("Syscall #%d is not implemented yet\n", syscall_id))
	}
}

func (vm *VM) Step(ops []Op, sc *ScriptContext) {
	op := ops[sc.Addr]
	switch op.Typ {
	case OpPushInt, OpPushBool:
		sc.Stack.Push(op.Operand)
		sc.Addr++
	case OpIf, OpDo:
		top := sc.Stack.Pop().(types.BoolType)
		if I2B(top) {
			sc.Addr++
		} else {
			sc.Addr += op.Operand.(types.IntType)
		}
	case OpElse, OpEnd, OpBreak, OpContinue:
		sc.Addr += op.Operand.(types.IntType)
	case OpWhile:
		sc.Addr++
	case OpIntrinsic:
		intrinsic := op.Operand.(lexer.IntrinsicType)
		switch intrinsic {
		case lexer.IntrinsicPlus, lexer.IntrinsicMinus, lexer.IntrinsicMul, lexer.IntrinsicBitAnd, lexer.IntrinsicBitOr, lexer.IntrinsicBitXor:
			b := sc.Stack.Pop().(types.IntType)
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(SafeArithmeticFunctions[intrinsic](a, b))
		case lexer.IntrinsicDiv:
			b := sc.Stack.Pop().(types.IntType)
			if b == 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, "Division by zero")
			}
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(a / b)
		case lexer.IntrinsicMod:
			b := sc.Stack.Pop().(types.IntType)
			if b == 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, "Division by zero")
			}
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(a % b)
		case lexer.IntrinsicShl:
			b := sc.Stack.Pop().(types.IntType)
			if b < 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Negative shift amount in `<<`: %d", b))
			}
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(a << b)
		case lexer.IntrinsicShr:
			b := sc.Stack.Pop().(types.IntType)
			if b < 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Negative shift amount in `>>`: %d", b))
			}
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(a >> b)
		case lexer.IntrinsicBitNot:
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(^a)
		case lexer.IntrinsicLogicalAnd, lexer.IntrinsicLogicalOr:
			b := sc.Stack.Pop().(types.BoolType)
			a := sc.Stack.Pop().(types.BoolType)
			sc.Stack.Push(LogicalFunctions[intrinsic](a, b))
		case lexer.IntrinsicLogicalNot:
			x := sc.Stack.Pop()
			sc.Stack.Push(B2I(!I2B(x.(types.BoolType))))
		case lexer.IntrinsicDup:
			x := sc.Stack.Top()
			sc.Stack.Push(x)
		case lexer.IntrinsicSwap:
			b := sc.Stack.Pop()
			a := sc.Stack.Pop()
			sc.Stack.Push(b)
			sc.Stack.Push(a)
		case lexer.IntrinsicDrop:
			sc.Stack.Pop()
		case lexer.IntrinsicOver:
			x := sc.Stack.Data[len(sc.Stack.Data)-2]
			sc.Stack.Push(x)
		case lexer.IntrinsicRot:
			c := sc.Stack.Pop()
			b := sc.Stack.Pop()
			a := sc.Stack.Pop()
			sc.Stack.Push(b)
			sc.Stack.Push(c)
			sc.Stack.Push(a)
		case lexer.IntrinsicEq, lexer.IntrinsicNe, lexer.IntrinsicLe, lexer.IntrinsicGe, lexer.IntrinsicLt, lexer.IntrinsicGt:
			b := sc.Stack.Pop().(types.IntType)
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(ComparableFunctions[intrinsic](a, b))
		case lexer.IntrinsicPuti:
			x := sc.Stack.Pop()
			fmt.Print(x)
		case lexer.IntrinsicDebug:
			fmt.Printf(
				"\tMem: %v\tStack: %v\n",
				vm.Ctx.Memory.Data[vm.Ctx.Memory.OperativeMemRegion.Start:vm.Ctx.Memory.OperativeMemRegion.Ptr],
				sc.Stack.Data,
			)
		case lexer.IntrinsicLoad8, lexer.IntrinsicLoad16, lexer.IntrinsicLoad32, lexer.IntrinsicLoad64:
			ptr := sc.Stack.Pop().(types.IntType)
			val := vm.Ctx.Memory.LoadFromMem(ptr, LoadSizes[intrinsic])
			sc.Stack.Push(val)
		case lexer.IntrinsicStore8, lexer.IntrinsicStore16, lexer.IntrinsicStore32, lexer.IntrinsicStore64:
			ptr := sc.Stack.Pop().(types.IntType)
			x := sc.Stack.Pop().(types.IntType)
			vm.Ctx.Memory.StoreToMem(ptr, x, StoreSizes[intrinsic])

		case lexer.IntrinsicArgc:
			sc.Stack.Push(types.IntType(len(sc.Args)))
		case lexer.IntrinsicArgv:
			sc.Stack.Push(vm.Ctx.Memory.Argv)
		case lexer.IntrinsicSyscall:
			vm.ProcessSyscall(sc)
		default:
			lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled intrinsic: `%s`", op.OpToken.Text))
		}
		sc.Addr++
	case OpCall:
		if sc.ReturnStack.Size() >= vm.RecursionLimit {
			lexer.RuntimeFatal(&op.OpToken.Loc, "Recursion limit exceeded")
		}
		sc.ReturnStack.Push(sc.Addr)
		sc.Addr += op.Operand.(types.IntType)
	case OpFuncBegin:
		vm.Ctx.Memory.OperativeMemRegion.Ptr += op.Operand.(types.IntType)
		sc.Addr++
		if sc.debug {
			sc.ScopeStack.Push(op.DebugInfo.(string))
		}
	case OpFuncEnd:
		if sc.ReturnStack.Size() == 0 {
			lexer.RuntimeFatal(&op.OpToken.Loc, "Return stack is empty")
		}
		sc.Addr = sc.ReturnStack.Pop().(types.IntType) + 1
		vm.Ctx.Memory.OperativeMemRegion.Ptr -= op.Operand.(types.IntType)
		if sc.debug {
			sc.ScopeStack.Pop()
		}
	case OpPushLocalAlloc:
		addr := vm.Ctx.Memory.OperativeMemRegion.Ptr - op.Operand.(types.IntType)
		sc.Stack.Push(addr)
		sc.Addr++
	case OpPushGlobalAlloc:
		addr := vm.Ctx.Memory.OperativeMemRegion.Start + op.Operand.(types.IntType)
		sc.Stack.Push(addr)
		sc.Addr++
	default:
		lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled operation: `%s`", OpName[op.Typ]))
	}
}

func (vm *VM) Prepare(ops []Op, args []string, debug bool) *ScriptContext {
	vm.Ctx.Memory.Prepare(args)

	len_ops := types.IntType(len(ops))
	sc := NewScriptContext(len_ops, args, debug)
	sc.Addr = vm.Ctx.Funcs["main"].Addr

	vm.Ctx.Memory.OperativeMemRegion.Ptr = vm.Ctx.Memory.OperativeMemRegion.Start + vm.Ctx.GlobalScope().MemSize
	return sc
}

func (vm *VM) Interprete(ops []Op, args []string) ExitCodeType {

	sc := vm.Prepare(ops, args, false)
	for sc.Addr < sc.OpsCount {
		vm.Step(ops, sc)
	}
	return sc.GetExitCode(ops)
}

func (vm *VM) InterpreteDebug(ops []Op, args []string, di *DebugInterface) {
	sc := vm.Prepare(ops, args, true)

loop:
	for {
		cmd := <-di.Commands

		switch cmd.Type {
		case DebugCmdStack: // print stack
			fmt.Println(sc.Stack.Data)
			di.SendOK()
		case DebugCmdMemory: // print memory
			vm.Ctx.Memory.PrintDebug()
			di.SendOK()
		case DebugCmdOperativeMemory:
			chunk := cmd.Args.([]int)
			start, size := chunk[0], chunk[1]
			if start >= int(vm.Ctx.Memory.MemorySize) {
				di.SendFailed(fmt.Sprintf("Start address %d is out of bounds", start))
				continue
			}
			end := start + size
			if end >= int(vm.Ctx.Memory.MemorySize) {
				end = int(vm.Ctx.Memory.MemorySize) - 1
			}
			memory_chunk := vm.Ctx.Memory.Data[start:end]
			fmt.Printf("addr=%d size=%d: %v\n", start, size, memory_chunk)
			di.SendOK()
		case DebugCmdPrint:
			names := cmd.Args.([]string)
			scope_name := sc.ScopeStack.Top().(string)
			n_found := vm.Ctx.DebugConstNames(names, scope_name) +
				vm.Ctx.DebugAllocNames(names, scope_name) +
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
			if sc.Addr >= sc.OpsCount {
				di.SendFailed("Can not step: script finished")
			} else {
				steps_count := cmd.Args.(int)
				for i := 0; i < steps_count && sc.Addr < sc.OpsCount; i++ {
					vm.Step(ops, sc)

					addr, is_bp := di.IsBreakpoint(sc, ops)
					if is_bp {
						fmt.Printf("[INFO] Break at address %d\n", addr)
						break
					}
				}
				if sc.Addr >= sc.OpsCount {
					ec := sc.GetExitCode(ops)
					fmt.Printf("[INFO] Script finished with exit code %d", ec.Code)
					if len(ec.Msg) > 0 {
						fmt.Printf(": %s", ec.Msg)
					}
					fmt.Println()
				}
				di.SendOK()
			}
		case DebugCmdContinue: // continue
			if sc.Addr >= sc.OpsCount {
				di.SendFailed("Can not continue: script finished")
			} else {
				for sc.Addr < sc.OpsCount {
					vm.Step(ops, sc)

					addr, is_bp := di.IsBreakpoint(sc, ops)
					if is_bp {
						fmt.Printf("[INFO] Break at address %d\n", addr)
						break
					}
				}
				if sc.Addr >= sc.OpsCount {
					ec := sc.GetExitCode(ops)
					fmt.Printf("[INFO] Script finished with exit code %d", ec.Code)
					if len(ec.Msg) > 0 {
						fmt.Printf(": %s", ec.Msg)
					}
					fmt.Println()
				}
				di.SendOK()
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
				if addr >= sc.OpsCount || addr < 0 {
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
			if sc.Addr >= sc.OpsCount {
				di.SendFailed("Can not print token: script finished")
			} else {
				token := ops[sc.Addr].OpToken
				fmt.Printf("%s:%d:%d Token(%s)\n", token.Loc.Filepath, token.Loc.Line+1, token.Loc.Column+1, token.Text)
				di.SendOK()
			}
		case DebugCmdOperation: // operation
			if sc.Addr >= sc.OpsCount {
				di.SendFailed("Can not print operation: script finished")
			} else {
				context_size := cmd.Args.(types.IntType)
				di.PrintOpsList(sc.Addr-context_size, sc.Addr+context_size, ops, sc)
				di.SendOK()
			}
		case DebugCmdOperationList: // print ops list
			di.PrintOpsList(0, sc.OpsCount-1, ops, sc)
			di.SendOK()
		case DebugCmdEnv: // env - consts and allocs
			typ := cmd.Args.(string)
			if sc.Addr >= sc.OpsCount {
				di.SendFailed("Can not print environment: script finished")
			} else {
				current_scope_name := sc.ScopeStack.Top().(string)

				if typ == "all" || typ == "local" {
					fmt.Printf("Scope: <%s>\n", current_scope_name)
					if current_scope_name != GlobalScopeName {
						fmt.Printf("Locals: ")
						vm.Ctx.DebugConsts(current_scope_name)
						vm.Ctx.DebugAllocs(current_scope_name)
						fmt.Println()
					}
				}

				if typ == "all" || typ == "global" {
					fmt.Printf("Globals: ")
					vm.Ctx.DebugConsts(GlobalScopeName)
					vm.Ctx.DebugAllocs(GlobalScopeName)
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
