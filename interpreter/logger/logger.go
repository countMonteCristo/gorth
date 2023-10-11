package logger

import (
	"Gorth/interpreter/utils"
	"fmt"
	"os"
)

func formatLevelErr(loc *utils.Location, m ModuleType, l LogLevelType, msg string, args ...any) error {
	path := ""
	if loc != nil {
		path = fmt.Sprintf(":\n  %s:%d:%d", loc.Filepath, loc.Line+1, loc.Column+1)
	}
	return fmt.Errorf("[%s] [%s] %s%s", LogLevelToStr(l), ModuleToStr(m), fmt.Sprintf(msg, args...), path)
}

func formatErrMsg(loc *utils.Location, m ModuleType, msg string, args ...any) error {
	return formatLevelErr(loc, m, Error, msg, args...)
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

func VmRuntimeError(loc *utils.Location, msg string, args ...any) error {
	return formatLevelErr(loc, ModuleVm, RuntimeError, msg, args...)
}

func TypeCheckerError(loc *utils.Location, msg string, args ...any) error {
	return formatErrMsg(loc, ModuleTypeChecker, msg, args...)
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

// func Crash(loc *utils.Location, msg string, args ...any) {
// 	err := VmRuntimeError(loc, msg, args...)
// 	fmt.Fprintln(os.Stderr, err.Error())
// 	utils.Exit(1)
// }

func crash(loc *utils.Location, m ModuleType, msg string, args ...any) {
	err := formatCrashMsg(loc, m, msg, args...)
	fmt.Fprintln(os.Stderr, err.Error())
	utils.Exit(1)
}

func formatCrashMsg(loc *utils.Location, m ModuleType, msg string, args ...any) error {
	return formatLevelErr(loc, m, Fatal, msg, args...)
}

func LexerCrash(loc *utils.Location, msg string, args ...any) {
	crash(loc, ModuleLexer, msg, args...)
}

func CompilerCrash(loc *utils.Location, msg string, args ...any) {
	crash(loc, ModuleCompiler, msg, args...)
}

func TypeCheckerCrash(loc *utils.Location, msg string, args ...any) {
	crash(loc, ModuleTypeChecker, msg, args...)
}

func VmCrash(loc *utils.Location, msg string, args ...any) {
	crash(loc, ModuleVm, msg, args...)
}
