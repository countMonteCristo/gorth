package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
	"unsafe"

	"golang.org/x/sys/unix"
)

// ---------------------------------------------------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------------------------------------------------

func (vm *VM) ProcessSyscall0() {
	switch syscall_id := uintptr(vm.Rc.Stack.Pop()); syscall_id {
	case unix.SYS_SCHED_YIELD:
		r1, _, err := unix.Syscall(syscall_id, 0, 0, 0)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_GETPID:
		r1, _, err := unix.Syscall(syscall_id, 0, 0, 0)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_FORK:
		r1, _, err := unix.Syscall(syscall_id, 0, 0, 0)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	default:
		logger.VmCrash(nil, "syscall0 for #%d is not implemented yet\n", syscall_id)
	}
}

func (vm *VM) ProcessSyscall1() {
	switch syscall_id := uintptr(vm.Rc.Stack.Pop()); syscall_id {
	case unix.SYS_CLOSE:
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), 0, 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_EXIT:
		utils.Exit(int(vm.Rc.Stack.Pop()))
	case unix.SYS_FSYNC:
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), 0, 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_FDATASYNC:
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), 0, 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_PIPE:
		fds := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[fds])), 0, 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_DUP:
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), 0, 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	default:
		logger.VmCrash(nil, "syscall1 for #%d is not implemented yet\n", syscall_id)
	}
}

func (vm *VM) ProcessSyscall2() {
	switch syscall_id := uintptr(vm.Rc.Stack.Pop()); syscall_id {
	case unix.SYS_STAT:
		statbuf := vm.Rc.Stack.Pop()
		pathname := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id,
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[pathname])),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[statbuf])),
			0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_FSTAT:
		statbuf := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id,
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[fd])),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[statbuf])),
			0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_LSTAT:
		statbuf := vm.Rc.Stack.Pop()
		pathname := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id,
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[pathname])),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[statbuf])),
			0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_ACCESS:
		pathname := vm.Rc.Stack.Pop()
		mode := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id,
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[pathname])),
			uintptr(mode),
			0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_DUP2:
		oldfd := vm.Rc.Stack.Pop()
		newfd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(oldfd), uintptr(newfd), 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_LISTEN:
		backlog := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), uintptr(backlog), 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_NANOSLEEP:
		rem_ptr := vm.Rc.Stack.Pop()
		req_ptr := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id,
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[req_ptr])),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[rem_ptr])),
			0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_KILL:
		signal := vm.Rc.Stack.Pop()
		pid := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(pid), uintptr(signal), 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_SHUTDOWN:
		how := vm.Rc.Stack.Pop()
		sockfd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(sockfd), uintptr(how), 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_FLOCK:
		cmd := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), uintptr(cmd), 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_FDATASYNC:
		length := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), uintptr(length), 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	default:
		logger.VmCrash(nil, "syscall2 for #%d is not implemented yet\n", syscall_id)
	}
}

func (vm *VM) ProcessSyscall3() {
	switch syscall_id := uintptr(vm.Rc.Stack.Pop()); syscall_id {
	case unix.SYS_OPEN:
		mode := vm.Rc.Stack.Pop()
		flags := vm.Rc.Stack.Pop()
		ptr := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[ptr])), uintptr(flags), uintptr(mode),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_READ:
		count := vm.Rc.Stack.Pop()
		ptr := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[ptr])), uintptr(count),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_WRITE:
		count := vm.Rc.Stack.Pop()
		ptr := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[ptr])), uintptr(count),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_POLL:
		timeoout := vm.Rc.Stack.Pop()
		nfds := vm.Rc.Stack.Pop()
		fds := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[fds])), uintptr(nfds), uintptr(timeoout),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_LSEEK:
		whence := vm.Rc.Stack.Pop()
		offset := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), uintptr(offset), uintptr(whence),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_IOCTL:
		arg := vm.Rc.Stack.Pop()
		request := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), uintptr(request), uintptr(arg),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_READV:
		iovcnt := vm.Rc.Stack.Pop()
		iov := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[iov])), uintptr(iovcnt),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_WRITEV:
		iovcnt := vm.Rc.Stack.Pop()
		iov := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[iov])), uintptr(iovcnt),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_SOCKET:
		protocol := vm.Rc.Stack.Pop()
		typ := vm.Rc.Stack.Pop()
		domain := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(domain), uintptr(typ), uintptr(protocol),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_BIND:
		addrlen := vm.Rc.Stack.Pop()
		addr := vm.Rc.Stack.Pop()
		sock_fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(sock_fd), uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[addr])), uintptr(addrlen),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_ACCEPT:
		addrlen_ptr := vm.Rc.Stack.Pop()
		addr := vm.Rc.Stack.Pop()
		sock_fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(sock_fd), uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[addr])), uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[addrlen_ptr])),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_FCNTL:
		arg := vm.Rc.Stack.Pop()
		cmd := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall(
			syscall_id, uintptr(fd), uintptr(arg), uintptr(cmd),
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	default:
		logger.VmCrash(nil, "syscall3 for #%d is not implemented yet\n", syscall_id)
	}
}

func (vm *VM) ProcessSyscall4() {
	switch syscall_id := uintptr(vm.Rc.Stack.Pop()); syscall_id {
	case unix.SYS_PREAD64:
		offset := vm.Rc.Stack.Pop()
		count := vm.Rc.Stack.Pop()
		buf_ptr := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall6(
			syscall_id, uintptr(fd),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[buf_ptr])),
			uintptr(count),
			uintptr(offset),
			0, 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_PWRITE64:
		offset := vm.Rc.Stack.Pop()
		count := vm.Rc.Stack.Pop()
		buf_ptr := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall6(
			syscall_id, uintptr(fd),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[buf_ptr])),
			uintptr(count),
			uintptr(offset),
			0, 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_WAIT4:
		rusage_ptr := vm.Rc.Stack.Pop()
		options := vm.Rc.Stack.Pop()
		wstatus_ptr := vm.Rc.Stack.Pop()
		pid := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall6(
			syscall_id, uintptr(pid),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[wstatus_ptr])),
			uintptr(options),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[rusage_ptr])),
			0, 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	default:
		logger.VmCrash(nil, "syscall4 for #%d is not implemented yet\n", syscall_id)
	}
}

func (vm *VM) ProcessSyscall5() {
	switch syscall_id := uintptr(vm.Rc.Stack.Pop()); syscall_id {
	case unix.SYS_SETSOCKOPT:
		optlen := vm.Rc.Stack.Pop()
		optval_ptr := vm.Rc.Stack.Pop()
		optname := vm.Rc.Stack.Pop()
		level := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall6(
			syscall_id, uintptr(fd), uintptr(level), uintptr(optname),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[optval_ptr])),
			uintptr(optlen), 0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	case unix.SYS_SELECT:
		timeout_ptr := vm.Rc.Stack.Pop()
		except_fds := vm.Rc.Stack.Pop()
		output_fds := vm.Rc.Stack.Pop()
		input_fds := vm.Rc.Stack.Pop()
		n := vm.Rc.Stack.Pop()
		r1, _, err := unix.Syscall6(
			syscall_id, uintptr(n),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[input_fds])),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[output_fds])),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[except_fds])),
			uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[timeout_ptr])),
			0,
		)
		vm.Rc.Stack.Push(types.IntType(r1))
		vm.Rc.Stack.Push(types.IntType(err))
	default:
		logger.VmCrash(nil, "syscall2 for #%d is not implemented yet\n", syscall_id)
	}
}

func (vm *VM) ProcessSyscall6() {
	switch syscall_id := uintptr(vm.Rc.Stack.Pop()); syscall_id {
	default:
		logger.VmCrash(nil, "syscall6 for #%d is not implemented yet\n", syscall_id)
	}
}

// ---------------------------------------------------------------------------------------------------------------------

func (vm *VM) Step(ops *[]Op) (err error) {
	op := &(*ops)[vm.Rc.Addr]
	switch op.Typ {
	case OpPushInt, OpPushBool, OpPushPtr:
		vm.Rc.Stack.Push(op.Operand.(types.IntType))
		vm.Rc.Addr++
	case OpCondJump:
		top := vm.Rc.Stack.Pop()
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
			b := vm.Rc.Stack.Pop()
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(SafeArithmeticFunctions[intrinsic](a, b))
		case lexer.IntrinsicDiv:
			b := vm.Rc.Stack.Pop()
			if b == 0 {
				return logger.VmRuntimeError(&op.Token.Loc, "Division by zero")
			}
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(a / b)
		case lexer.IntrinsicMod:
			b := vm.Rc.Stack.Pop()
			if b == 0 {
				return logger.VmRuntimeError(&op.Token.Loc, "Division by zero")
			}
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(a % b)
		case lexer.IntrinsicShl:
			b := vm.Rc.Stack.Pop()
			if b < 0 {
				return logger.VmRuntimeError(&op.Token.Loc, "Negative shift amount in `<<`: %d", b)
			}
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(a << b)
		case lexer.IntrinsicShr:
			b := vm.Rc.Stack.Pop()
			if b < 0 {
				return logger.VmRuntimeError(&op.Token.Loc, "Negative shift amount in `>>`: %d", b)
			}
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(a >> b)
		case lexer.IntrinsicBitNot:
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(^a)
		case lexer.IntrinsicLogicalAnd, lexer.IntrinsicLogicalOr:
			b := vm.Rc.Stack.Pop()
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(LogicalFunctions[intrinsic](a, b))
		case lexer.IntrinsicLogicalNot:
			x := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(B2I(!I2B(x)))
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
			b := vm.Rc.Stack.Pop()
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(ComparableFunctions[intrinsic](a, b))
		case lexer.IntrinsicPuti:
			x := vm.Rc.Stack.Pop()
			fmt.Print(x)
		case lexer.IntrinsicDebug:
			fmt.Printf(
				"\tMem: %v\tStack: %v\n",
				// vm.Rc.Memory.Data[vm.Rc.Memory.Ram.Start:vm.Rc.Memory.Ram.Ptr],
				0,
				vm.Rc.Stack.Data,
			)
		case lexer.IntrinsicTypeDebug:
			// do nothing
		case lexer.IntrinsicLoad8, lexer.IntrinsicLoad16, lexer.IntrinsicLoad32, lexer.IntrinsicLoad64:
			ptr := vm.Rc.Stack.Pop()
			val, err := vm.Rc.Memory.LoadFromMem(ptr, LoadSizes[intrinsic], &op.Token.Loc, false)
			if err != nil {
				return err
			}
			vm.Rc.Stack.Push(val)
		case lexer.IntrinsicStore8, lexer.IntrinsicStore16, lexer.IntrinsicStore32, lexer.IntrinsicStore64:
			ptr := vm.Rc.Stack.Pop()
			x := vm.Rc.Stack.Pop()
			if err = vm.Rc.Memory.StoreToMem(ptr, x, StoreSizes[intrinsic], &op.Token.Loc, false); err != nil {
				return err
			}

		case lexer.IntrinsicArgc:
			vm.Rc.Stack.Push(vm.Rc.Argc)
		case lexer.IntrinsicArgv:
			vm.Rc.Stack.Push(vm.Rc.Memory.Argv)
		case lexer.IntrinsicEnv:
			vm.Rc.Stack.Push(vm.Rc.Memory.Env)

		case lexer.IntrinsicSyscall0:
			vm.ProcessSyscall0()
		case lexer.IntrinsicSyscall1:
			vm.ProcessSyscall1()
		case lexer.IntrinsicSyscall2:
			vm.ProcessSyscall2()
		case lexer.IntrinsicSyscall3:
			vm.ProcessSyscall3()
		case lexer.IntrinsicSyscall4:
			vm.ProcessSyscall4()
		case lexer.IntrinsicSyscall5:
			vm.ProcessSyscall5()
		case lexer.IntrinsicSyscall6:
			vm.ProcessSyscall6()

		case lexer.IntrinsicCastInt, lexer.IntrinsicCastPtr, lexer.IntrinsicCastBool:
			// do nothing

		default:
			return logger.VmRuntimeError(&op.Token.Loc, "Unhandled intrinsic: `%s`", op.Token.Text)
		}
		vm.Rc.Addr++
	case OpCall:
		if vm.Rc.ReturnStack.Size() >= int(vm.S.CallStackSize) {
			return logger.VmRuntimeError(&op.Token.Loc, "Call stack overflow")
		}
		vm.Rc.ReturnStack.Push(vm.Rc.Addr)
		vm.Rc.ReturnStack.Push(vm.Rc.CapturesCount)
		vm.Rc.Addr += op.Operand.(types.IntType)
	case OpFuncBegin:
		vm.Rc.Memory.Ram.Ptr += op.Operand.(types.IntType)
		vm.Rc.CapturesCount = 0
		vm.Rc.Addr++
		if vm.S.Debug {
			vm.Rc.Scopes.Push(op.DebugInfo.(string))
		}
	case OpFuncEnd:
		if vm.Rc.ReturnStack.Empty() {
			return logger.VmRuntimeError(&op.Token.Loc, "Return stack is empty")
		}
		for i := types.IntType(0); i < vm.Rc.CapturesCount; i++ {
			vm.Rc.ReturnStack.Pop()
		}
		vm.Rc.CapturesCount = vm.Rc.ReturnStack.Pop()
		vm.Rc.Addr = vm.Rc.ReturnStack.Pop() + 1
		vm.Rc.Memory.Ram.Ptr -= op.Operand.(types.IntType)
		if vm.S.Debug {
			vm.Rc.Scopes.Pop()
		}
	case OpPushLocalAlloc:
		addr := vm.Rc.Memory.Ram.Ptr - op.Operand.(types.IntType)
		vm.Rc.Stack.Push(addr)
		vm.Rc.Addr++
	case OpPushGlobalAlloc:
		addr := vm.Rc.Memory.Ram.Start + op.Operand.(types.IntType)
		vm.Rc.Stack.Push(addr)
		vm.Rc.Addr++
	case OpCapture:
		cap_count := op.Operand.(types.IntType)
		for i := types.IntType(0); i < cap_count; i++ {
			x := vm.Rc.Stack.Data[types.IntType(vm.Rc.Stack.Size())-cap_count+i]
			vm.Rc.ReturnStack.Push(x)
		}
		vm.Rc.CapturesCount += cap_count
		vm.Rc.Addr++
	case OpDropCaptures:
		cap_count := op.Operand.(types.IntType)
		for i := types.IntType(0); i < cap_count; i++ {
			vm.Rc.ReturnStack.Pop()
		}
		vm.Rc.CapturesCount -= cap_count
		vm.Rc.Addr++
	case OpPushCaptured:
		idx := op.Operand.(types.IntType)
		x := vm.Rc.ReturnStack.Data[types.IntType(vm.Rc.ReturnStack.Size())-1-idx]
		vm.Rc.Stack.Push(x)
		vm.Rc.Addr++
	default:
		return logger.VmRuntimeError(&op.Token.Loc, "Unhandled operation: `%s`", OpType2Str[op.Typ])
	}
	return
}

// ---------------------------------------------------------------------------------------------------------------------

func (vm *VM) PrepareRuntimeContext(args []string) {
	vm.Rc.PrepareMemory(args, &vm.S)
	vm.Rc.Reset()
}

// ---------------------------------------------------------------------------------------------------------------------

func (vm *VM) Interprete(ops *[]Op, args []string) ExitCodeType {
	vm.PrepareRuntimeContext(args)
	var err error = nil
	for vm.Rc.Addr < vm.Rc.OpsCount {
		err = vm.Step(ops)
		if err != nil {
			break
		}
	}
	return vm.Rc.GetExitCode(err)
}

// ---------------------------------------------------------------------------------------------------------------------
