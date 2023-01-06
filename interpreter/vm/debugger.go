package vm

import (
	"Gorth/interpreter/types"
	"fmt"
	"strconv"
	"strings"
)

type DebugCommandType int

const (
	DebugCmdStep DebugCommandType = iota
	DebugCmdBreakpointSet
	DebugCmdBreakpointList
	DebugCmdBreakpointRemove
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
	"bs": DebugCmdBreakpointSet, "bl": DebugCmdBreakpointList, "br": DebugCmdBreakpointRemove,
	"t":  DebugCmdToken, "o": DebugCmdOperation, "ol": DebugCmdOperationList,
	"s": DebugCmdStack, "m": DebugCmdMemory, "e": DebugCmdEnv,
	"h": DebugCmdHelp, "q": DebugCmdQuit,
}

type BreakPointList struct {
	Funcs []string
	Addr  []types.IntType
}

func (l *BreakPointList) Count() int {
	return len(l.Funcs) + len(l.Addr)
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
	BreakPoints map[types.IntType]bool
}

func NewDebugInterface() *DebugInterface {
	return &DebugInterface{
		Commands: make(chan DebugCommand), Response: make(chan DebugCommandResponse),
		BreakPoints: make(map[types.IntType]bool),
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

func (di *DebugInterface) IsBreakpoint(ctx *ScriptContext, ops []Op) (types.IntType, bool) {
	_, exists := di.BreakPoints[ctx.Addr]
	if exists {
		return ctx.Addr, true
	}
	return -1, false
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
	case DebugCmdOperation:
		if len(parts) > 1 {
			arg, err := strconv.ParseInt(parts[1], 10, 64)
			if err != nil || arg < 0 {
				fmt.Printf("Argument of `step` command should be unsigned integer, but got %s. Use 0 by default.\n", parts[1])
				arg = 0
			}
			cmd.Args = arg
		} else {
			cmd.Args = types.IntType(0)
		}
	case DebugCmdBreakpointSet, DebugCmdBreakpointRemove:
		bps := BreakPointList{
			Funcs: make([]string, 0), Addr: make([]int64, 0),
		}
		for _, arg := range parts[1:] {
			addr, ok := strconv.ParseInt(arg, 10, 64)
			if ok == nil {
				bps.Addr = append(bps.Addr, addr)
			} else {
				bps.Funcs = append(bps.Funcs, arg)
			}
		}
		if bps.Count() == 0 {
			fmt.Printf("Specify names amd/or addresses for breakpoints\n")
			return cmd, false
		}
		cmd.Args = bps
	}
	return cmd, true
}
