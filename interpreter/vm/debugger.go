package vm

import "strings"

type DebugCommandType int

const (
	DebugCmdStep DebugCommandType = iota
	DebugCmdBreak
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
	"n": DebugCmdStep, "c": DebugCmdContinue, "t": DebugCmdToken, "o": DebugCmdOperation,
	"ol": DebugCmdOperationList, "s": DebugCmdStack, "m": DebugCmdMemory,
	"e": DebugCmdEnv, "h": DebugCmdHelp, "q": DebugCmdQuit,
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
	Commands chan DebugCommand
	Response chan DebugCommandResponse
}

func NewDebugInterface() *DebugInterface {
	return &DebugInterface{
		Commands: make(chan DebugCommand), Response: make(chan DebugCommandResponse),
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
	return cmd, true
}
