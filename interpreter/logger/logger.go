package logger

import (
	"Gorth/interpreter/utils"
	"fmt"
	"os"
)

func FormatErrMsg(loc *utils.Location, msg string, args ...any) error {
	path := ""
	if loc != nil {
		path = fmt.Sprintf(":\n  %s:%d:%d", loc.Filepath, loc.Line+1, loc.Column+1)
	}
	return fmt.Errorf("[%s] %s%s", LogMsgTypeToStr(Error), fmt.Sprintf(msg, args...), path)
}

func FormatRuntimeErrMsg(loc *utils.Location, msg string, args ...any) error {
	path := ""
	if loc != nil {
		path = fmt.Sprintf(":\n  %s:%d:%d", loc.Filepath, loc.Line+1, loc.Column+1)
	}
	return fmt.Errorf("[%s] %s%s", LogMsgTypeToStr(RuntimeError), fmt.Sprintf(msg, args...), path)
}

func FormatInfoMsg(loc *utils.Location, msg string, args ...any) string {
	path := ""
	if loc != nil {
		path = fmt.Sprintf(": %s:%d:%d", loc.Filepath, loc.Line+1, loc.Column+1)
	}
	return fmt.Sprintf("[%s] %s%s", LogMsgTypeToStr(Info), fmt.Sprintf(msg, args...), path)
}

func FormatNoneMsg(loc *utils.Location, msg string, args ...any) string {
	path := ""
	if loc != nil {
		path = fmt.Sprintf(": %s:%d:%d", loc.Filepath, loc.Line+1, loc.Column+1)
	}
	return fmt.Sprintf("%s%s", fmt.Sprintf(msg, args...), path)
}

func Crash(loc *utils.Location, msg string, args ...any) {
	err := FormatRuntimeErrMsg(loc, msg, args...)
	fmt.Fprintln(os.Stderr, err.Error())
	utils.Exit(1)
}
