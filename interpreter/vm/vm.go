package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"fmt"
	"unsafe"

	"golang.org/x/sys/unix"
)

type VM struct {
	Rc RunTimeContext
	S  VmSettings
}

func NewVM(s *VmSettings, global_scope_name string) *VM {
	vm := VM{
		S: *s,
	}
	vm.Rc = *NewRuntimeContext(&vm.S, global_scope_name)

	return &vm
}

func (vm *VM) ProcessSyscall1() {
	switch syscall_id := vm.Rc.Stack.Pop().(types.IntType); syscall_id {
	case unix.SYS_CLOSE:
		fd := vm.Rc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_CLOSE, uintptr(fd), 0, 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	default:
		logger.VmCrash(nil, "syscall1 for #%d is not implemented yet\n", syscall_id)
	}
}

func (vm *VM) ProcessSyscall3() {
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
		logger.VmCrash(nil, "syscall3 for #%d is not implemented yet\n", syscall_id)
	}
}

func (vm *VM) Step(ops []Op) (err error) {
	op := ops[vm.Rc.Addr]
	switch op.Typ {
	case OpPushInt, OpPushBool, OpPushPtr:
		vm.Rc.Stack.Push(op.Operand)
		vm.Rc.Addr++
	case OpCondJump:
		top := vm.Rc.Stack.Pop().(types.BoolType)
		if I2B(top) {
			vm.Rc.Addr++
		} else {
			vm.Rc.Addr += op.Operand.(types.IntType)
		}
	case OpJump:
		vm.Rc.Addr += op.Operand.(types.IntType)
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
				return logger.VmRuntimeError(&op.OpToken.Loc, "Division by zero")
			}
			a := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Stack.Push(a / b)
		case lexer.IntrinsicMod:
			b := vm.Rc.Stack.Pop().(types.IntType)
			if b == 0 {
				return logger.VmRuntimeError(&op.OpToken.Loc, "Division by zero")
			}
			a := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Stack.Push(a % b)
		case lexer.IntrinsicShl:
			b := vm.Rc.Stack.Pop().(types.IntType)
			if b < 0 {
				return logger.VmRuntimeError(&op.OpToken.Loc, "Negative shift amount in `<<`: %d", b)
			}
			a := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Stack.Push(a << b)
		case lexer.IntrinsicShr:
			b := vm.Rc.Stack.Pop().(types.IntType)
			if b < 0 {
				return logger.VmRuntimeError(&op.OpToken.Loc, "Negative shift amount in `>>`: %d", b)
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
		case lexer.IntrinsicTypeDebug:
			// do nothing
		case lexer.IntrinsicLoad8, lexer.IntrinsicLoad16, lexer.IntrinsicLoad32, lexer.IntrinsicLoad64:
			ptr := vm.Rc.Stack.Pop().(types.IntType)
			val := vm.Rc.Memory.LoadFromMem(ptr, LoadSizes[intrinsic])
			vm.Rc.Stack.Push(val)
		case lexer.IntrinsicStore8, lexer.IntrinsicStore16, lexer.IntrinsicStore32, lexer.IntrinsicStore64:
			ptr := vm.Rc.Stack.Pop().(types.IntType)
			x := vm.Rc.Stack.Pop().(types.IntType)
			vm.Rc.Memory.StoreToMem(ptr, x, StoreSizes[intrinsic])

		case lexer.IntrinsicArgc:
			vm.Rc.Stack.Push(vm.Rc.Argc)
		case lexer.IntrinsicArgv:
			vm.Rc.Stack.Push(vm.Rc.Memory.Argv)
		case lexer.IntrinsicEnv:
			vm.Rc.Stack.Push(vm.Rc.Memory.Env)

		case lexer.IntrinsicSyscall1:
			vm.ProcessSyscall1()
		case lexer.IntrinsicSyscall3:
			vm.ProcessSyscall3()

		case lexer.IntrinsicCastInt, lexer.IntrinsicCastPtr, lexer.IntrinsicCastBool:
			// do nothing

		default:
			return logger.VmRuntimeError(&op.OpToken.Loc, "Unhandled intrinsic: `%s`", op.OpToken.Text)
		}
		vm.Rc.Addr++
	case OpCall:
		if vm.Rc.ReturnStack.Size() >= int(vm.S.CallStackSize) {
			return logger.VmRuntimeError(&op.OpToken.Loc, "Call stack overflow")
		}
		vm.Rc.ReturnStack.Push(vm.Rc.Addr)
		vm.Rc.Addr += op.Operand.(types.IntType)
	case OpFuncBegin:
		vm.Rc.Memory.OperativeMemRegion.Ptr += op.Operand.(types.IntType)
		vm.Rc.Addr++
		if vm.S.Debug {
			vm.Rc.ScopeStack.Push(op.DebugInfo.(string))
		}
	case OpFuncEnd:
		if vm.Rc.ReturnStack.Empty() {
			return logger.VmRuntimeError(&op.OpToken.Loc, "Return stack is empty")
		}
		vm.Rc.Addr = vm.Rc.ReturnStack.Pop().(types.IntType) + 1
		vm.Rc.Memory.OperativeMemRegion.Ptr -= op.Operand.(types.IntType)
		if vm.S.Debug {
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
		return logger.VmRuntimeError(&op.OpToken.Loc, "Unhandled operation: `%s`", OpName[op.Typ])
	}
	return
}

func (vm *VM) PrepareRuntimeContext(ops []Op, args []string) {
	vm.Rc.PrepareMemory(args, &vm.S)
	vm.Rc.Reset()
}

func (vm *VM) Interprete(ops []Op, args []string) ExitCodeType {
	vm.PrepareRuntimeContext(ops, args)
	var err error = nil
	for vm.Rc.Addr < vm.Rc.OpsCount {
		err = vm.Step(ops)
		if err != nil {
			break
		}
	}
	return vm.Rc.GetExitCode(ops, err)
}
