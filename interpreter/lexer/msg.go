package lexer

import (
	"Gorth/interpreter/utils"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
)

var (
	_, msg_file_path, _, _ = runtime.Caller(0)
	package_dir   = filepath.Dir(filepath.Dir(filepath.Dir(msg_file_path)))
)

// TODO: add CompilerWarn

func LogStatus(loc *Location, file string, line int, msg, status string) {
	rel_file := file[len(package_dir)+1:]
	fmt.Fprintf(os.Stderr, "%s:%d [%s] %s:\n", rel_file, line, status, msg)
	fmt.Fprintf(os.Stderr, "  %s:%d:%d\n", loc.Filepath, loc.Line+1, loc.Column+1)
}

func CompilerFatal(loc *Location, msg string) {
	_, file, line, _ := runtime.Caller(1)
	LogStatus(loc, file, line, msg, "ERROR")
	utils.Exit(1)
}

func CompilerInfo(loc *Location, msg string) {
	_, file, line, _ := runtime.Caller(1)
	LogStatus(loc, file, line, msg, "INFO")
}

func RuntimeFatal(loc *Location, msg string) {
	_, file, line, _ := runtime.Caller(1)
	LogStatus(loc, file, line, msg, "RUNTIME_ERROR")
	os.Exit(1)
}
