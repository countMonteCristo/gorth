package vm

import (
	"fmt"
	"strconv"
	"strings"
)

type DebugCommandType int

const (
	DebugCmdStep DebugCommandType = iota
	DebugCmdBreakpoint
	DebugCmdContinue
	DebugCmdStack
	DebugCmdMemory
	DebugCmdEnv
	DebugCmdToken
	DebugCmdOperation
	DebugCmdOperationList
	DebugCmdHelp
	DebugCmdQuit
)

var Str2DebugCommandType = map[string]DebugCommandType{
	"n": DebugCmdStep, "c": DebugCmdContinue,
	"b": DebugCmdBreakpoint,
	"t": DebugCmdToken, "o": DebugCmdOperation, "ol": DebugCmdOperationList,
	"s": DebugCmdStack, "m": DebugCmdMemory, "e": DebugCmdEnv,
	"h": DebugCmdHelp, "q": DebugCmdQuit,
}

type DebugCommandStatusType int

const (
	DebugCommandStatusOK DebugCommandStatusType = iota
	DebugCommandStatusFailed
)

type DebugCommand struct {
	Type DebugCommandType
	Str  string
	Args interface{}
}

type DebugCommandResponse struct {
	Status DebugCommandStatusType
	Msg    string
}

type DebugInterface struct {
	Commands    chan DebugCommand
	Response    chan DebugCommandResponse
	BreakPoints map[string]bool
}

func NewDebugInterface() *DebugInterface {
	return &DebugInterface{
		Commands: make(chan DebugCommand), Response: make(chan DebugCommandResponse),
		BreakPoints: make(map[string]bool),
	}
}

func (di *DebugInterface) Communicate(cmd DebugCommand) DebugCommandResponse {
	di.Commands <- cmd
	return <-di.Response
}

func (di *DebugInterface) SendOK() {
	di.Response <- DebugCommandResponse{Status: DebugCommandStatusOK}
}
func (di *DebugInterface) SendFailed(msg string) {
	di.Response <- DebugCommandResponse{
		Status: DebugCommandStatusFailed, Msg: msg,
	}
}

func (di *DebugInterface) IsBreakpoint(ctx *ScriptContext, ops []Op) (string, bool) {
	if ctx.Addr < ctx.OpsCount {
		op := ops[ctx.Addr]
		if op.Typ == OpFuncBegin {
			fn_name := op.Operand.(string)
			_, exists := di.BreakPoints[fn_name]
			if exists {
				return fn_name, true
			}
		}
	}
	return "", false
}

func ParseDebuggerCommand(input string) (DebugCommand, bool) {
	cmd := DebugCommand{Str: input}
	parts := strings.Fields(input)
	if len(parts) == 0 {
		return cmd, false
	}
	cmd_type, exists := Str2DebugCommandType[parts[0]]
	if !exists {
		return cmd, false
	}

	cmd.Type = cmd_type

	// TODO: save other args from parts[1:] to cmd.Args
	switch cmd.Type {
	case DebugCmdStep:
		if len(parts) > 1 {
			arg, err := strconv.Atoi(parts[1])
			if err != nil || arg <= 0 {
				fmt.Printf("Argument of `step` command should be unsigned integer > 0, but got %s. Use 1 by default.\n", parts[1])
				arg = 1
			}
			cmd.Args = arg
		} else {
			cmd.Args = 1
		}
	case DebugCmdBreakpoint:
		if len(parts) < 2 {
			fmt.Printf("Specify names for breakpoints")
			return cmd, false
		}
		cmd.Args = parts[1:]
	}
	return cmd, true
}
