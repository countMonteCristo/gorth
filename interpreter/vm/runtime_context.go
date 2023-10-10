package vm

import (
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
	"os"
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
}

func NewRuntimeContext(s *Settings) *RunTimeContext {
	rc := &RunTimeContext{
		Memory: InitMemory(s.MemorySize),
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

func (rc *RunTimeContext) Prepare(ctx *CompileTimeContext, args []string, ops_count types.IntType, s *Settings) {

	env := []string{}
	if s.Env {
		env = os.Environ()
	}

	rc.OpsCount = ops_count
	rc.ReturnStack.Push(rc.OpsCount) // for exit after hitting OpFuncEnd of main function

	rc.Args = args                                // for OpArgc
	rc.Memory.Prepare(args, env, &ctx.StringsMap) // load string literals and input args to memory

	// rc.Addr = ctx.Funcs["main"].Addr // script entry point address
	rc.Addr = ops_count - 1 // script entry point address (last op should be OpCall to main)

	// in case we have global allocs
	rc.Memory.OperativeMemRegion.Ptr = rc.Memory.OperativeMemRegion.Start + ctx.GlobalScope().MemSize
}

func (rc *RunTimeContext) GetExitCode(ops []Op, err error) ExitCodeType {
	if err == nil {
		switch {
		case rc.Stack.Size() > 1:
			rc.ExitCode.Code = types.IntType(1)
			rc.ExitCode.Msg = fmt.Sprintf("Multiple values left in stack after script exit: %v", rc.Stack.Data)
		case rc.Stack.Empty():
			rc.ExitCode.Code = types.IntType(2)
			rc.ExitCode.Msg = "Empty stack after script exit"
		default:
			rc.ExitCode.Code = rc.Stack.Pop().(types.IntType)
		}
	} else {
		rc.ExitCode.Code = types.IntType(3)
		rc.ExitCode.Msg = err.Error()
	}

	return rc.ExitCode
}
