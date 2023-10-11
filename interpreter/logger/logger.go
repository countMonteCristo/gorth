package logger

import (
	"Gorth/interpreter/utils"
	"fmt"
	"os"
)

func formatErrMsg(loc *utils.Location, m ModuleType, msg string, args ...any) error {
	path := ""
	if loc != nil {
		path = fmt.Sprintf(":\n  %s:%d:%d", loc.Filepath, loc.Line+1, loc.Column+1)
	}
	return fmt.Errorf("[%s] [%s] %s%s", LogLevelToStr(Error), ModuleToStr(m), fmt.Sprintf(msg, args...), path)
}

func LexerError(loc *utils.Location, msg string, args ...any) error {
	return formatErrMsg(loc, ModuleLexer, msg, args...)
}

func CompilerError(loc *utils.Location, msg string, args ...any) error {
	return formatErrMsg(loc, ModuleCompiler, msg, args...)
}

func VmError(loc *utils.Location, msg string, args ...any) error {
	return formatErrMsg(loc, ModuleVm, msg, args...)
}

func TypeCheckerError(loc *utils.Location, msg string, args ...any) error {
	return formatErrMsg(loc, ModuleTypeChecker, msg, args...)
}

func FormatTCErrMsg(loc *utils.Location, msg string, args ...any) error {
	path := ""
	if loc != nil {
		path = fmt.Sprintf(":\n  %s:%d:%d", loc.Filepath, loc.Line+1, loc.Column+1)
	}
	return fmt.Errorf("[%s] [TypeCheck] %s%s", LogLevelToStr(Error), fmt.Sprintf(msg, args...), path)
}

func FormatRuntimeErrMsg(loc *utils.Location, msg string, args ...any) error {
	path := ""
	if loc != nil {
		path = fmt.Sprintf(":\n  %s:%d:%d", loc.Filepath, loc.Line+1, loc.Column+1)
	}
	return fmt.Errorf("[%s] %s%s", LogLevelToStr(RuntimeError), fmt.Sprintf(msg, args...), path)
}

func FormatInfoMsg(loc *utils.Location, msg string, args ...any) string {
	path := ""
	if loc != nil {
		path = fmt.Sprintf(": %s:%d:%d", loc.Filepath, loc.Line+1, loc.Column+1)
	}
	return fmt.Sprintf("[%s] %s%s", LogLevelToStr(Info), fmt.Sprintf(msg, args...), path)
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
