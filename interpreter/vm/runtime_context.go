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
