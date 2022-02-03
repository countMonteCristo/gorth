package lexer

import (
	"fmt"
	"os"
)

func CompilerFatal(loc *Location, msg string) {
	fmt.Printf("%s:%d:%d: ERROR: %s\n", loc.Filepath, loc.Line+1, loc.Column+1, msg)
}

func CompilerInfo(loc *Location, msg string) {
	fmt.Printf("%s:%d:%d: INFO: %s\n", loc.Filepath, loc.Line+1, loc.Column+1, msg)
}

func RuntimeFatal(loc *Location, msg string) {
	fmt.Printf("%s:%d:%d: RuntimeError: %s\n", loc.Filepath, loc.Line+1, loc.Column+1, msg)
	os.Exit(1)
}
