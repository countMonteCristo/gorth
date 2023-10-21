package debugger

import (
	"Gorth/interpreter/types"
	"fmt"
	"strconv"
	"strings"
)

// ---------------------------------------------------------------------------------------------------------------------

type DebugCommandType int

const (
	DebugCmdStep DebugCommandType = iota
	DebugCmdBreakpointSet
	DebugCmdBreakpointList
	DebugCmdBreakpointRemove
	DebugCmdContinue
	DebugCmdUp
	DebugCmdStack
	DebugCmdMemory
	DebugCmdOperativeMemory
	DebugCmdPrint
	DebugCmdEnv
	DebugCmdToken
	DebugCmdOperation
	DebugCmdOperationList
	DebugCmdHelp
	DebugCmdQuit
	DebugCmdRestart
)

var Str2DebugCommandType = map[string]DebugCommandType{
	"n": DebugCmdStep, "c": DebugCmdContinue, "u": DebugCmdUp,
	"bs": DebugCmdBreakpointSet, "bl": DebugCmdBreakpointList, "br": DebugCmdBreakpointRemove,
	"t": DebugCmdToken, "o": DebugCmdOperation, "ol": DebugCmdOperationList,
	"s": DebugCmdStack, "e": DebugCmdEnv,
	"m": DebugCmdMemory, "mo": DebugCmdOperativeMemory,
	"p": DebugCmdPrint,
	"h": DebugCmdHelp, "q": DebugCmdQuit,
	"r": DebugCmdRestart,
}

// ---------------------------------------------------------------------------------------------------------------------

type DebugTransition int

const (
	DebugRestart DebugTransition = iota
	DebugQuit
)

// ---------------------------------------------------------------------------------------------------------------------

type DebugCommand struct {
	Type    DebugCommandType
	Name    string
	Args    interface{}
	ArgList []string
}

func NewDebugCommand(input string) *DebugCommand {
	parts := strings.Fields(input)
	if len(parts) == 0 {
		return nil
	}

	cmd_name, arg_list := parts[0], parts[1:]
	cmd_type, exists := Str2DebugCommandType[cmd_name]
	if !exists {
		return nil
	}

	return &DebugCommand{Type: cmd_type, Name: cmd_name, ArgList: arg_list}
}

// ---------------------------------------------------------------------------------------------------------------------

type DebugCommandStatusType int

const (
	DebugCommandStatusOK DebugCommandStatusType = iota
	DebugCommandStatusFailed
)

// ---------------------------------------------------------------------------------------------------------------------

type DebugCommandResponse struct {
	Status DebugCommandStatusType
	Msg    string
}

// ---------------------------------------------------------------------------------------------------------------------

func ParseDebuggerCommand(input string) (*DebugCommand, bool) {
	cmd := NewDebugCommand(input)
	if cmd == nil {
		return cmd, false
	}

	switch cmd.Type {
	case DebugCmdStep:
		if len(cmd.ArgList) > 0 {
			arg, err := strconv.Atoi(cmd.ArgList[0])
			if err != nil || arg <= 0 {
				fmt.Printf("Argument of `n` command should be unsigned integer > 0, but got %s. Use 1 by default.\n", cmd.ArgList[0])
				arg = 1
			}
			cmd.Args = arg
		} else {
			cmd.Args = 1
		}
	case DebugCmdOperation:
		if len(cmd.ArgList) > 0 {
			arg, err := strconv.ParseInt(cmd.ArgList[0], 10, 64)
			if err != nil || arg < 0 {
				fmt.Printf("Argument of `o` command should be unsigned integer, but got %s. Use 0 by default.\n", cmd.ArgList[0])
				arg = 0
			}
			cmd.Args = arg
		} else {
			cmd.Args = types.IntType(0)
		}
	case DebugCmdBreakpointSet, DebugCmdBreakpointRemove:
		bps := BreakPointList{
			Funcs: make([]string, 0), Addr: make([]types.IntType, 0),
		}
		for _, arg := range cmd.ArgList {
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
	case DebugCmdOperativeMemory:
		if len(cmd.ArgList) < 2 {
			fmt.Printf("Specify start address and size of memory chunk\n")
			return cmd, false
		}
		start, err := strconv.Atoi(cmd.ArgList[0])
		if err != nil || start < 0 {
			fmt.Printf("Start address should be unsigned integer, got `%s`\n", cmd.ArgList[0])
			return cmd, false
		}
		size, err := strconv.Atoi(cmd.ArgList[1])
		if err != nil || size < 0 {
			fmt.Printf("Size of memory chunk should be unsigned integer, got `%s`\n", cmd.ArgList[1])
			return cmd, false
		}
		cmd.Args = []int{start, size}
	case DebugCmdPrint:
		if len(cmd.ArgList) < 1 {
			fmt.Println("Specify names for constants, allocs or functions to print")
			return cmd, false
		}
		cmd.Args = cmd.ArgList
	case DebugCmdEnv:
		if len(cmd.ArgList) == 0 {
			cmd.Args = "all"
		} else {
			if cmd.ArgList[0] == "local" || cmd.ArgList[0] == "global" || cmd.ArgList[0] == "all" {
				cmd.Args = cmd.ArgList[0]
			} else {
				fmt.Printf("Unknown parameter for `e` command: %s, only [all, local, global] are supported\n", cmd.ArgList[0])
				return cmd, false
			}
		}
	}

	return cmd, true
}

// ---------------------------------------------------------------------------------------------------------------------
