package vm

import (
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
	"os"
)

// ---------------------------------------------------------------------------------------------------------------------

type ExitCodeType struct {
	Code types.IntType
	Msg  string
}

// ---------------------------------------------------------------------------------------------------------------------

type RuntimeSettings struct {
	StringLiterals      *map[string]types.IntType
	GlobalMemorySize    types.IntType
	EntryPointAddr      types.IntType
	StringLiteralsStart types.IntType
	OpsCount            types.IntType
}

func NewRuntimeSettings(start types.IntType) *RuntimeSettings {
	return &RuntimeSettings{
		StringLiteralsStart: start,
	}
}

// ---------------------------------------------------------------------------------------------------------------------

type RunTimeContext struct {
	Memory        Memory
	Stack         utils.Stack
	ReturnStack   utils.Stack
	CapturesCount types.IntType
	ScopeStack    utils.Stack
	Addr          types.IntType
	Argc          types.IntType
	OpsCount      types.IntType
	ExitCode      ExitCodeType
	Settings      RuntimeSettings
	global_scope  string
}

func NewRuntimeContext(s *VmSettings, global_scope_name string) *RunTimeContext {
	rc := &RunTimeContext{
		Memory: NewMemory(s.MemorySize),
		Stack:  utils.Stack{}, ReturnStack: utils.Stack{}, CapturesCount: 0,
		Addr: 0, ScopeStack: utils.Stack{},
		ExitCode:     ExitCodeType{Code: 0},
		global_scope: global_scope_name,
	}
	rc.ScopeStack.Push(rc.global_scope)
	rc.Settings = *NewRuntimeSettings(rc.Memory.Strings.Start)
	return rc
}

// ---------------------------------------------------------------------------------------------------------------------

func (rc *RunTimeContext) Reset() {
	rc.Stack.Clear()
	rc.ReturnStack.Clear()
	rc.ScopeStack.Clear()
	rc.ScopeStack.Push(rc.global_scope)
	rc.OpsCount = rc.Settings.OpsCount

	rc.Addr = rc.Settings.EntryPointAddr
	rc.ReturnStack.Push(rc.OpsCount) // for exit after hitting OpFuncEnd of main function
}

// ---------------------------------------------------------------------------------------------------------------------

func (rc *RunTimeContext) PrepareMemory(args []string, s *VmSettings) {
	env := []string{}
	if s.Env {
		env = os.Environ()
	}

	rc.Argc = types.IntType(len(args))                       // for OpArgc
	rc.Memory.Prepare(args, env, rc.Settings.StringLiterals) // load string literals and input args to memory

	// in case we have global allocs
	rc.Memory.Ram.Ptr = rc.Memory.Ram.Start + rc.Settings.GlobalMemorySize
}

// ---------------------------------------------------------------------------------------------------------------------

func (rc *RunTimeContext) GetExitCode(err error) ExitCodeType {
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

// ---------------------------------------------------------------------------------------------------------------------
