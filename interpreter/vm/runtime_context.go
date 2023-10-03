package vm

import (
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
)

type ExitCodeType struct {
	Code types.IntType
	Msg  string
}

type RunTimeContext struct {
	Memory      ByteMemory
	Stack       utils.Stack
	ReturnStack utils.Stack
	ScopeStack  utils.Stack
	Addr        types.IntType
	Args        []string
	OpsCount    types.IntType
	ExitCode    ExitCodeType
	debug       bool
}

func NewRuntimeContext(memSize types.IntType) *RunTimeContext {
	rc := &RunTimeContext{
		Memory: InitMemory(memSize),
		Stack:  utils.Stack{}, ReturnStack: utils.Stack{},
		Addr: 0, ScopeStack: utils.Stack{},
		ExitCode: ExitCodeType{Code: 0},
	}
	rc.ScopeStack.Push(GlobalScopeName)
	return rc
}

func (rc *RunTimeContext) Argc() types.IntType {
	return types.IntType(len(rc.Args))
}

func (rc *RunTimeContext) Prepare(ctx *CompileTimeContext, args []string, ops_count types.IntType, debug bool) {
	rc.OpsCount = ops_count
	rc.ReturnStack.Push(rc.OpsCount) // for exit after hitting OpFuncEnd of main function

	rc.Args = args                           // for OpArgc
	rc.Memory.Prepare(args, &ctx.StringsMap) // load string literals and input args to memory

	rc.debug = debug                 // debug mode
	rc.Addr = ctx.Funcs["main"].Addr // script entry point address

	// in case we have global allocs
	rc.Memory.OperativeMemRegion.Ptr = rc.Memory.OperativeMemRegion.Start + ctx.GlobalScope().MemSize
}

func (rc *RunTimeContext) GetExitCode(ops []Op, err error) ExitCodeType {
	switch {
	case rc.Stack.Size() > 1:
		rc.ExitCode.Code = types.IntType(1)
		rc.ExitCode.Msg = fmt.Sprintf("Multiple values left in stack after script exit: %v", rc.Stack.Data)
	case rc.Stack.Size() == 0:
		rc.ExitCode.Code = types.IntType(2)
		rc.ExitCode.Msg = "Empty stack after script exit"
	default:
		if err != nil {
			rc.ExitCode.Code = types.IntType(3)
			rc.ExitCode.Msg = err.Error()
		} else {
			rc.ExitCode.Code = rc.Stack.Pop().(types.IntType)
		}
	}
	return rc.ExitCode
}
