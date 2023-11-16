package vm

import (
	"Gorth/interpreter/intrinsics"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/operations"
	"Gorth/interpreter/settings"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
	"syscall"
	"unsafe"
)

// ---------------------------------------------------------------------------------------------------------------------

type VM struct {
	Rc RunTimeContext
}

func NewVM(s *settings.Settings, global_scope_name string) *VM {
	vm := VM{
		Rc: *NewRuntimeContext(s, global_scope_name),
	}

	return &vm
}

// ---------------------------------------------------------------------------------------------------------------------

func (vm *VM) mPtr(ptr types.IntType) uintptr {
	return uintptr(unsafe.Pointer(&vm.Rc.Memory.Data[ptr]))
}

// Gets pointer to the zero-terminated array of pointers and return slice of pointers
func (vm *VM) ptrSlice(ptr types.IntType) []*byte {
	ptrs := make([]*byte, 0)
	for {
		p, err := vm.Rc.Memory.LoadFromMem(ptr, SIZEOF_PTR, nil, true)
		if err != nil {
			logger.VmCrash(nil, "Cannot load pointer from memory")
		}
		if p == 0 {
			ptrs = append(ptrs, nil)
			break
		}
		ptrs = append(ptrs, &vm.Rc.Memory.Data[ptr])
		ptr += SIZEOF_PTR
	}
	return ptrs
}

// ---------------------------------------------------------------------------------------------------------------------

func (vm *VM) ProcessSyscall0() {
	var r1 uintptr        // syscall result
	var err syscall.Errno // errno

	syscall_id := uintptr(vm.Rc.Stack.Pop())
	switch syscall_id {
	case syscall.SYS_SCHED_YIELD:
	case syscall.SYS_GETPID:
	case syscall.SYS_FORK:
	default:
		logger.VmCrash(nil, "syscall0 for #%d is not implemented yet\n", syscall_id)
	}

	r1, _, err = syscall.Syscall(syscall_id, 0, 0, 0)
	vm.Rc.Stack.Push(types.IntType(r1))
	vm.Rc.Stack.Push(types.IntType(err))
}

func (vm *VM) ProcessSyscall1() {
	var r1 uintptr        // syscall result
	var err syscall.Errno // errno
	var a1 uintptr        // syscall input arguments

	syscall_id := uintptr(vm.Rc.Stack.Pop())
	switch syscall_id {
	case syscall.SYS_CLOSE:
		fd := vm.Rc.Stack.Pop()
		a1 = uintptr(fd)
	case syscall.SYS_PIPE:
		fds := vm.Rc.Stack.Pop()
		a1 = vm.mPtr(fds)
	case syscall.SYS_DUP:
		fd := vm.Rc.Stack.Pop()
		a1 = uintptr(fd)
	case syscall.SYS_EXIT:
		utils.Exit(int(vm.Rc.Stack.Pop()))
	case syscall.SYS_FSYNC:
		fd := vm.Rc.Stack.Pop()
		a1 = uintptr(fd)
	case syscall.SYS_FDATASYNC:
		fd := vm.Rc.Stack.Pop()
		a1 = uintptr(fd)
	default:
		logger.VmCrash(nil, "syscall1 for #%d is not implemented yet\n", syscall_id)
	}

	r1, _, err = syscall.Syscall(syscall_id, a1, 0, 0)
	vm.Rc.Stack.Push(types.IntType(r1))
	vm.Rc.Stack.Push(types.IntType(err))
}

func (vm *VM) ProcessSyscall2() {
	var r1 uintptr        // syscall result
	var err syscall.Errno // errno
	var a1, a2 uintptr    // syscall input arguments

	syscall_id := uintptr(vm.Rc.Stack.Pop())
	switch syscall_id {
	case syscall.SYS_STAT:
		statbuf := vm.Rc.Stack.Pop()
		pathname := vm.Rc.Stack.Pop()
		a1, a2 = vm.mPtr(pathname), vm.mPtr(statbuf)
	case syscall.SYS_FSTAT:
		statbuf := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2 = vm.mPtr(fd), vm.mPtr(statbuf)
	case syscall.SYS_LSTAT:
		statbuf := vm.Rc.Stack.Pop()
		pathname := vm.Rc.Stack.Pop()
		a1, a2 = vm.mPtr(pathname), vm.mPtr(statbuf)
	case syscall.SYS_ACCESS:
		pathname := vm.Rc.Stack.Pop()
		mode := vm.Rc.Stack.Pop()
		a1, a2 = vm.mPtr(pathname), uintptr(mode)
	case syscall.SYS_DUP2:
		oldfd := vm.Rc.Stack.Pop()
		newfd := vm.Rc.Stack.Pop()
		a1, a2 = uintptr(oldfd), uintptr(newfd)
	case syscall.SYS_NANOSLEEP:
		rem_ptr := vm.Rc.Stack.Pop()
		req_ptr := vm.Rc.Stack.Pop()
		a1, a2 = vm.mPtr(req_ptr), vm.mPtr(rem_ptr)
	case syscall.SYS_SHUTDOWN:
		how := vm.Rc.Stack.Pop()
		sockfd := vm.Rc.Stack.Pop()
		a1, a2 = uintptr(sockfd), uintptr(how)
	case syscall.SYS_LISTEN:
		backlog := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2 = uintptr(fd), uintptr(backlog)
	case syscall.SYS_KILL:
		signal := vm.Rc.Stack.Pop()
		pid := vm.Rc.Stack.Pop()
		a1, a2 = uintptr(pid), uintptr(signal)
	case syscall.SYS_FLOCK:
		cmd := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2 = uintptr(fd), uintptr(cmd)
	case syscall.SYS_FDATASYNC:
		length := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2 = uintptr(fd), uintptr(length)
	default:
		logger.VmCrash(nil, "syscall2 for #%d is not implemented yet\n", syscall_id)
	}

	r1, _, err = syscall.Syscall(syscall_id, a1, a2, 0)
	vm.Rc.Stack.Push(types.IntType(r1))
	vm.Rc.Stack.Push(types.IntType(err))
}

func (vm *VM) ProcessSyscall3() {
	var r1 uintptr         // syscall result
	var err syscall.Errno  // errno
	var a1, a2, a3 uintptr // syscall input arguments

	syscall_id := uintptr(vm.Rc.Stack.Pop())
	switch syscall_id {
	case syscall.SYS_READ:
		count := vm.Rc.Stack.Pop()
		ptr := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(fd), vm.mPtr(ptr), uintptr(count)
	case syscall.SYS_WRITE:
		count := vm.Rc.Stack.Pop()
		ptr := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(fd), vm.mPtr(ptr), uintptr(count)
	case syscall.SYS_OPEN:
		mode := vm.Rc.Stack.Pop()
		flags := vm.Rc.Stack.Pop()
		ptr := vm.Rc.Stack.Pop()
		a1, a2, a3 = vm.mPtr(ptr), uintptr(flags), uintptr(mode)
	case syscall.SYS_POLL:
		timeoout := vm.Rc.Stack.Pop()
		nfds := vm.Rc.Stack.Pop()
		fds := vm.Rc.Stack.Pop()
		a1, a2, a3 = vm.mPtr(fds), uintptr(nfds), uintptr(timeoout)
	case syscall.SYS_LSEEK:
		whence := vm.Rc.Stack.Pop()
		offset := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(fd), uintptr(offset), uintptr(whence)
	case syscall.SYS_IOCTL:
		arg := vm.Rc.Stack.Pop()
		request := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(fd), uintptr(request), uintptr(arg)
	case syscall.SYS_READV:
		iovcnt := vm.Rc.Stack.Pop()
		iov := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(fd), vm.mPtr(iov), uintptr(iovcnt)
	case syscall.SYS_WRITEV:
		iovcnt := vm.Rc.Stack.Pop()
		iov := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(fd), vm.mPtr(iov), uintptr(iovcnt)
	case syscall.SYS_SOCKET:
		protocol := vm.Rc.Stack.Pop()
		typ := vm.Rc.Stack.Pop()
		domain := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(domain), uintptr(typ), uintptr(protocol)
	case syscall.SYS_CONNECT:
		addr_len := vm.Rc.Stack.Pop()
		addr_ptr := vm.Rc.Stack.Pop()
		sock_fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(sock_fd), vm.mPtr(addr_ptr), uintptr(addr_len)
	case syscall.SYS_ACCEPT:
		addrlen_ptr := vm.Rc.Stack.Pop()
		addr_ptr := vm.Rc.Stack.Pop()
		sock_fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(sock_fd), vm.mPtr(addr_ptr), vm.mPtr(addrlen_ptr)
	case syscall.SYS_SENDMSG:
		flags := vm.Rc.Stack.Pop()
		msg_ptr := vm.Rc.Stack.Pop()
		sock_fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(sock_fd), vm.mPtr(msg_ptr), uintptr(flags)
	case syscall.SYS_RECVMSG:
		flags := vm.Rc.Stack.Pop()
		msg_ptr := vm.Rc.Stack.Pop()
		sock_fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(sock_fd), vm.mPtr(msg_ptr), uintptr(flags)
	case syscall.SYS_BIND:
		addrlen := vm.Rc.Stack.Pop()
		addr := vm.Rc.Stack.Pop()
		sock_fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(sock_fd), vm.mPtr(addr), uintptr(addrlen)
	case syscall.SYS_GETSOCKNAME:
		addrlen_ptr := vm.Rc.Stack.Pop()
		addr_ptr := vm.Rc.Stack.Pop()
		sock_fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(sock_fd), vm.mPtr(addr_ptr), vm.mPtr(addrlen_ptr)
	case syscall.SYS_GETPEERNAME:
		addrlen_ptr := vm.Rc.Stack.Pop()
		addr_ptr := vm.Rc.Stack.Pop()
		sock_fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(sock_fd), vm.mPtr(addr_ptr), vm.mPtr(addrlen_ptr)
	case syscall.SYS_EXECVE:
		env_ptr := vm.Rc.Stack.Pop()
		args_ptr := vm.Rc.Stack.Pop()
		prog_ptr := vm.Rc.Stack.Pop()

		argp := vm.ptrSlice(args_ptr)
		envp := vm.ptrSlice(env_ptr)

		a1, a2, a3 = vm.mPtr(prog_ptr), uintptr(unsafe.Pointer(&argp[0])), uintptr(unsafe.Pointer(&envp[0]))
	case syscall.SYS_FCNTL:
		arg := vm.Rc.Stack.Pop()
		cmd := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2, a3 = uintptr(fd), uintptr(arg), uintptr(cmd)
	default:
		logger.VmCrash(nil, "syscall3 for #%d is not implemented yet\n", syscall_id)
	}

	r1, _, err = syscall.Syscall(syscall_id, a1, a2, a3)
	vm.Rc.Stack.Push(types.IntType(r1))
	vm.Rc.Stack.Push(types.IntType(err))
}

func (vm *VM) ProcessSyscall4() {
	var r1 uintptr             // syscall result
	var err syscall.Errno      // errno
	var a1, a2, a3, a4 uintptr // syscall input arguments

	syscall_id := uintptr(vm.Rc.Stack.Pop())
	switch syscall_id {
	case syscall.SYS_PREAD64:
		offset := vm.Rc.Stack.Pop()
		count := vm.Rc.Stack.Pop()
		buf_ptr := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2, a3, a4 = uintptr(fd), vm.mPtr(buf_ptr), uintptr(count), uintptr(offset)
	case syscall.SYS_PWRITE64:
		offset := vm.Rc.Stack.Pop()
		count := vm.Rc.Stack.Pop()
		buf_ptr := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2, a3, a4 = uintptr(fd), vm.mPtr(buf_ptr), uintptr(count), uintptr(offset)
	case syscall.SYS_SENDFILE:
		count := vm.Rc.Stack.Pop()
		offset_ptr := vm.Rc.Stack.Pop()
		in_fd := vm.Rc.Stack.Pop()
		out_fd := vm.Rc.Stack.Pop()
		a1, a2, a3, a4 = uintptr(out_fd), uintptr(in_fd), vm.mPtr(offset_ptr), uintptr(count)
	case syscall.SYS_SOCKETPAIR:
		sv_ptr := vm.Rc.Stack.Pop()
		protocol := vm.Rc.Stack.Pop()
		typ := vm.Rc.Stack.Pop()
		domain := vm.Rc.Stack.Pop()
		a1, a2, a3, a4 = uintptr(domain), uintptr(typ), uintptr(protocol), vm.mPtr(sv_ptr)
	case syscall.SYS_WAIT4:
		rusage_ptr := vm.Rc.Stack.Pop()
		options := vm.Rc.Stack.Pop()
		wstatus_ptr := vm.Rc.Stack.Pop()
		pid := vm.Rc.Stack.Pop()
		a1, a2, a3, a4 = uintptr(pid), vm.mPtr(wstatus_ptr), uintptr(options), vm.mPtr(rusage_ptr)
	default:
		logger.VmCrash(nil, "syscall4 for #%d is not implemented yet\n", syscall_id)
	}

	r1, _, err = syscall.Syscall6(syscall_id, a1, a2, a3, a4, 0, 0)
	vm.Rc.Stack.Push(types.IntType(r1))
	vm.Rc.Stack.Push(types.IntType(err))
}

func (vm *VM) ProcessSyscall5() {
	var r1 uintptr                 // syscall result
	var err syscall.Errno          // errno
	var a1, a2, a3, a4, a5 uintptr // syscall input arguments

	syscall_id := uintptr(vm.Rc.Stack.Pop())
	switch syscall_id {
	case syscall.SYS_SELECT:
		timeout_ptr := vm.Rc.Stack.Pop()
		except_fds := vm.Rc.Stack.Pop()
		output_fds := vm.Rc.Stack.Pop()
		input_fds := vm.Rc.Stack.Pop()
		n := vm.Rc.Stack.Pop()
		a1, a2, a3, a4, a5 = uintptr(n), vm.mPtr(input_fds), vm.mPtr(output_fds), vm.mPtr(except_fds), vm.mPtr(timeout_ptr)
	case syscall.SYS_SETSOCKOPT:
		optlen := vm.Rc.Stack.Pop()
		optval_ptr := vm.Rc.Stack.Pop()
		optname := vm.Rc.Stack.Pop()
		level := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2, a3, a4, a5 = uintptr(fd), uintptr(level), uintptr(optname), vm.mPtr(optval_ptr), uintptr(optlen)
	case syscall.SYS_GETSOCKOPT:
		optlen_ptr := vm.Rc.Stack.Pop()
		optval_ptr := vm.Rc.Stack.Pop()
		optname := vm.Rc.Stack.Pop()
		level := vm.Rc.Stack.Pop()
		fd := vm.Rc.Stack.Pop()
		a1, a2, a3, a4, a5 = uintptr(fd), uintptr(level), uintptr(optname), vm.mPtr(optval_ptr), vm.mPtr(optlen_ptr)
	default:
		logger.VmCrash(nil, "syscall2 for #%d is not implemented yet\n", syscall_id)
	}

	r1, _, err = syscall.Syscall6(syscall_id, a1, a2, a3, a4, a5, 0)
	vm.Rc.Stack.Push(types.IntType(r1))
	vm.Rc.Stack.Push(types.IntType(err))
}

func (vm *VM) ProcessSyscall6() {
	var r1 uintptr                     // syscall result
	var err syscall.Errno              // errno
	var a1, a2, a3, a4, a5, a6 uintptr // syscall input arguments

	syscall_id := uintptr(vm.Rc.Stack.Pop())
	switch syscall_id {
	case syscall.SYS_SENDTO:
		addrlen := vm.Rc.Stack.Pop()
		dest_addr_ptr := vm.Rc.Stack.Pop()
		flags := vm.Rc.Stack.Pop()
		len := vm.Rc.Stack.Pop()
		buf_ptr := vm.Rc.Stack.Pop()
		sock_fd := vm.Rc.Stack.Pop()
		a1, a2, a3, a4, a5, a6 = uintptr(sock_fd), vm.mPtr(buf_ptr), uintptr(len), uintptr(flags), vm.mPtr(dest_addr_ptr), uintptr(addrlen)
	case syscall.SYS_RECVFROM:
		addrlen_ptr := vm.Rc.Stack.Pop()
		dest_addr_ptr := vm.Rc.Stack.Pop()
		flags := vm.Rc.Stack.Pop()
		len := vm.Rc.Stack.Pop()
		buf_ptr := vm.Rc.Stack.Pop()
		sock_fd := vm.Rc.Stack.Pop()
		a1, a2, a3, a4, a5, a6 = uintptr(sock_fd), vm.mPtr(buf_ptr), uintptr(len), uintptr(flags), vm.mPtr(dest_addr_ptr), vm.mPtr(addrlen_ptr)
	default:
		logger.VmCrash(nil, "syscall6 for #%d is not implemented yet\n", syscall_id)
	}

	r1, _, err = syscall.Syscall6(syscall_id, a1, a2, a3, a4, a5, a6)
	vm.Rc.Stack.Push(types.IntType(r1))
	vm.Rc.Stack.Push(types.IntType(err))
}

// ---------------------------------------------------------------------------------------------------------------------

func (vm *VM) Step(ops *[]operations.Op, s *settings.Settings) (err error) {
	op := &(*ops)[vm.Rc.Addr]
	switch op.Typ {
	case operations.OpPushInt, operations.OpPushBool, operations.OpPushPtr, operations.OpPushFptr:
		vm.Rc.Stack.Push(op.Operand.(types.IntType))
		vm.Rc.Addr++
	case operations.OpCondJump:
		top := vm.Rc.Stack.Pop()
		if intrinsics.I2B(top) {
			vm.Rc.Addr++
		} else {
			vm.Rc.Addr += op.Operand.(types.IntType)
		}
	case operations.OpJump:
		vm.Rc.Addr += op.Operand.(types.IntType)
	case operations.OpIntrinsic:
		intrinsic := op.Operand.(intrinsics.IntrinsicType)
		switch intrinsic {
		case intrinsics.IntrinsicPlus, intrinsics.IntrinsicMinus, intrinsics.IntrinsicMul, intrinsics.IntrinsicBitAnd, intrinsics.IntrinsicBitOr, intrinsics.IntrinsicBitXor:
			b := vm.Rc.Stack.Pop()
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(intrinsics.SafeArithmeticFunctions[intrinsic](a, b))
		case intrinsics.IntrinsicDiv:
			b := vm.Rc.Stack.Pop()
			if b == 0 {
				return logger.VmRuntimeError(&op.Token.Loc, "Division by zero")
			}
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(a / b)
		case intrinsics.IntrinsicMod:
			b := vm.Rc.Stack.Pop()
			if b == 0 {
				return logger.VmRuntimeError(&op.Token.Loc, "Division by zero")
			}
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(a % b)
		case intrinsics.IntrinsicShl:
			b := vm.Rc.Stack.Pop()
			if b < 0 {
				return logger.VmRuntimeError(&op.Token.Loc, "Negative shift amount in `<<`: %d", b)
			}
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(a << b)
		case intrinsics.IntrinsicShr:
			b := vm.Rc.Stack.Pop()
			if b < 0 {
				return logger.VmRuntimeError(&op.Token.Loc, "Negative shift amount in `>>`: %d", b)
			}
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(a >> b)
		case intrinsics.IntrinsicBitNot:
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(^a)
		case intrinsics.IntrinsicLogicalAnd, intrinsics.IntrinsicLogicalOr:
			b := vm.Rc.Stack.Pop()
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(intrinsics.LogicalFunctions[intrinsic](a, b))
		case intrinsics.IntrinsicLogicalNot:
			x := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(intrinsics.B2I(!intrinsics.I2B(x)))
		case intrinsics.IntrinsicDup:
			x := vm.Rc.Stack.Top()
			vm.Rc.Stack.Push(x)
		case intrinsics.IntrinsicSwap:
			b := vm.Rc.Stack.Pop()
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(b)
			vm.Rc.Stack.Push(a)
		case intrinsics.IntrinsicDrop:
			vm.Rc.Stack.Pop()
		case intrinsics.IntrinsicOver:
			x := vm.Rc.Stack.Data[len(vm.Rc.Stack.Data)-2]
			vm.Rc.Stack.Push(x)
		case intrinsics.IntrinsicRot:
			c := vm.Rc.Stack.Pop()
			b := vm.Rc.Stack.Pop()
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(b)
			vm.Rc.Stack.Push(c)
			vm.Rc.Stack.Push(a)
		case intrinsics.IntrinsicEq, intrinsics.IntrinsicNe, intrinsics.IntrinsicLe, intrinsics.IntrinsicGe, intrinsics.IntrinsicLt, intrinsics.IntrinsicGt:
			b := vm.Rc.Stack.Pop()
			a := vm.Rc.Stack.Pop()
			vm.Rc.Stack.Push(intrinsics.ComparableFunctions[intrinsic](a, b))
		case intrinsics.IntrinsicPuti:
			x := vm.Rc.Stack.Pop()
			fmt.Print(x)
		case intrinsics.IntrinsicDebug:
			fmt.Printf(
				"\tMem: %v\tStack: %v\n",
				// vm.Rc.Memory.Data[vm.Rc.Memory.Ram.Start:vm.Rc.Memory.Ram.Ptr],
				0,
				vm.Rc.Stack.Data,
			)
		case intrinsics.IntrinsicTypeDebug:
			// do nothing
		case intrinsics.IntrinsicLoad8, intrinsics.IntrinsicLoad16, intrinsics.IntrinsicLoad32, intrinsics.IntrinsicLoad64:
			ptr := vm.Rc.Stack.Pop()
			val, err := vm.Rc.Memory.LoadFromMem(ptr, intrinsics.LoadSizes[intrinsic], &op.Token.Loc, false)
			if err != nil {
				return err
			}
			vm.Rc.Stack.Push(val)
		case intrinsics.IntrinsicStore8, intrinsics.IntrinsicStore16, intrinsics.IntrinsicStore32, intrinsics.IntrinsicStore64:
			ptr := vm.Rc.Stack.Pop()
			x := vm.Rc.Stack.Pop()
			if err = vm.Rc.Memory.StoreToMem(ptr, x, intrinsics.StoreSizes[intrinsic], &op.Token.Loc, false); err != nil {
				return err
			}

		case intrinsics.IntrinsicArgc:
			vm.Rc.Stack.Push(vm.Rc.Argc)
		case intrinsics.IntrinsicArgv:
			vm.Rc.Stack.Push(vm.Rc.Memory.Argv)
		case intrinsics.IntrinsicEnv:
			vm.Rc.Stack.Push(vm.Rc.Memory.Env)

		case intrinsics.IntrinsicSyscall0:
			vm.ProcessSyscall0()
		case intrinsics.IntrinsicSyscall1:
			vm.ProcessSyscall1()
		case intrinsics.IntrinsicSyscall2:
			vm.ProcessSyscall2()
		case intrinsics.IntrinsicSyscall3:
			vm.ProcessSyscall3()
		case intrinsics.IntrinsicSyscall4:
			vm.ProcessSyscall4()
		case intrinsics.IntrinsicSyscall5:
			vm.ProcessSyscall5()
		case intrinsics.IntrinsicSyscall6:
			vm.ProcessSyscall6()

		case intrinsics.IntrinsicCastInt, intrinsics.IntrinsicCastPtr, intrinsics.IntrinsicCastBool, intrinsics.IntrinsicCastFptr:
			// do nothing

		case intrinsics.IntrinsicAssert:
			size := vm.Rc.Stack.Pop()
			ptr := vm.Rc.Stack.Pop()
			cond := vm.Rc.Stack.Pop()
			if !intrinsics.I2B(cond) {
				return logger.VmRuntimeError(&op.Token.Loc, "ASSERT failed: `%s`", vm.Rc.Memory.Data[ptr:ptr+size])
			}

		default:
			return logger.VmRuntimeError(&op.Token.Loc, "Unhandled intrinsic: `%s`", op.Token.Text)
		}
		vm.Rc.Addr++
	case operations.OpCall:
		if vm.Rc.ReturnStack.Size() >= int(s.CallStackSize) {
			return logger.VmRuntimeError(&op.Token.Loc, "Call stack overflow")
		}
		vm.Rc.ReturnStack.Push(vm.Rc.Addr)
		vm.Rc.ReturnStack.Push(vm.Rc.CapturesCount)
		vm.Rc.Addr += op.Operand.(types.IntType)
	case operations.OpCallLike:
		addr := vm.Rc.Stack.Pop()
		if vm.Rc.ReturnStack.Size() >= int(s.CallStackSize) {
			return logger.VmRuntimeError(&op.Token.Loc, "Call stack overflow")
		}
		vm.Rc.ReturnStack.Push(vm.Rc.Addr)
		vm.Rc.ReturnStack.Push(vm.Rc.CapturesCount)
		vm.Rc.Addr = addr
	case operations.OpFuncBegin:
		vm.Rc.Memory.Ram.Ptr += op.Operand.(types.IntType)
		vm.Rc.CapturesCount = 0
		vm.Rc.Addr++
		if s.IsDebug() {
			vm.Rc.Scopes.Push(op.DebugInfo.(string))
		}
	case operations.OpFuncEnd:
		if vm.Rc.ReturnStack.Empty() {
			return logger.VmRuntimeError(&op.Token.Loc, "Return stack is empty")
		}
		for i := types.IntType(0); i < vm.Rc.CapturesCount; i++ {
			vm.Rc.ReturnStack.Pop()
		}
		vm.Rc.CapturesCount = vm.Rc.ReturnStack.Pop()
		vm.Rc.Addr = vm.Rc.ReturnStack.Pop() + 1
		vm.Rc.Memory.Ram.Ptr -= op.Operand.(types.IntType)
		if s.IsDebug() {
			vm.Rc.Scopes.Pop()
		}
	case operations.OpPushLocalAlloc:
		addr := vm.Rc.Memory.Ram.Ptr - op.Operand.(types.IntType)
		vm.Rc.Stack.Push(addr)
		vm.Rc.Addr++
	case operations.OpPushGlobalAlloc:
		addr := vm.Rc.Memory.Ram.Start + op.Operand.(types.IntType)
		vm.Rc.Stack.Push(addr)
		vm.Rc.Addr++
	case operations.OpCapture:
		cap_count := op.Operand.(types.IntType)
		for i := types.IntType(0); i < cap_count; i++ {
			x := vm.Rc.Stack.Data[types.IntType(vm.Rc.Stack.Size())-cap_count+i]
			vm.Rc.ReturnStack.Push(x)
		}
		vm.Rc.CapturesCount += cap_count
		vm.Rc.Addr++
	case operations.OpDropCaptures:
		cap_count := op.Operand.(types.IntType)
		for i := types.IntType(0); i < cap_count; i++ {
			vm.Rc.ReturnStack.Pop()
		}
		vm.Rc.CapturesCount -= cap_count
		vm.Rc.Addr++
	case operations.OpPushCaptured:
		idx := op.Operand.(types.IntType)
		x := vm.Rc.ReturnStack.Data[types.IntType(vm.Rc.ReturnStack.Size())-1-idx]
		vm.Rc.Stack.Push(x)
		vm.Rc.Addr++
	default:
		return logger.VmRuntimeError(&op.Token.Loc, "Unhandled operation: `%s`", operations.OpType2Str[op.Typ])
	}
	return
}

// ---------------------------------------------------------------------------------------------------------------------

func (vm *VM) PrepareRuntimeContext(args []string, s *settings.Settings) {
	vm.Rc.PrepareMemory(args, s)
	vm.Rc.Reset()
}

// ---------------------------------------------------------------------------------------------------------------------

func (vm *VM) Interprete(ops *[]operations.Op, args []string, s *settings.Settings) ExitCodeType {
	defer logger.Timeit(logger.ModuleVm, s.LogLevel)()

	vm.PrepareRuntimeContext(args, s)
	var err error = nil
	for vm.Rc.Addr < vm.Rc.OpsCount {
		err = vm.Step(ops, s)
		if err != nil {
			break
		}
	}
	return vm.Rc.GetExitCode(err)
}

// ---------------------------------------------------------------------------------------------------------------------
