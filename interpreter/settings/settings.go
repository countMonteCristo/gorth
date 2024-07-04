package settings

import (
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// ---------------------------------------------------------------------------------------------------------------------

type Settings struct {
	Mode          string // running mode
	Env           bool   // store env variables to memory
	TypeCheck     bool   // do type checking before running the program
	MemorySize    types.IntType
	CallStackSize int
	IncludePaths  utils.ArrayArgs
	Optimization  int
	ProfilePath   string
	LogLevel      logger.LogLevelType
	Dump          bool
}

// ---------------------------------------------------------------------------------------------------------------------

// TODO: add flags for memory and call_stack instead of reading env?
func NewSettings(
	mode string, env, typecheck bool, mem types.IntType, call_stack_Size int,
	include_paths utils.ArrayArgs, opt int, profile_fn string, loglevel string,
	dump bool) *Settings {
	value, exists := os.LookupEnv("GORTH_VM_MEMORY")
	if exists {
		val_int, err := strconv.ParseInt(value, 10, 64)
		if err != nil || val_int <= 0 {
			logger.Crash("Incorrect value for env variable GORTH_VM_MEMORY: %s", value)
		}
		mem = val_int
	}

	value, exists = os.LookupEnv("GORTH_VM_CALL_STACK")
	if exists {
		val_int, err := strconv.Atoi(value)
		if err != nil || val_int <= 0 {
			logger.Crash("Incorrect value for env variable GORTH_VM_CALL_STACK: %s", value)
		}
		call_stack_Size = val_int
	}

	var l logger.LogLevelType
	switch loglevel {
	case "error":
		l = logger.Error
	case "warn":
		l = logger.Warning
	case "debug":
		l = logger.Debug
	case "info":
		l = logger.Info
	default:
		logger.Crash("Incorrect value for log level: %s", loglevel)
	}

	return &Settings{
		Mode: mode, Env: env, TypeCheck: typecheck, MemorySize: mem, CallStackSize: call_stack_Size,
		IncludePaths: include_paths, Optimization: opt, ProfilePath: profile_fn, LogLevel: l,
		Dump: dump,
	}
}

func (s *Settings) IsDebug() bool {
	return s.Mode == "debug"
}

// ---------------------------------------------------------------------------------------------------------------------

func (s *Settings) Serialize() string {
	builder := strings.Builder{}

	builder.WriteString(fmt.Sprintf("%s\n", s.Mode))
	builder.WriteString(fmt.Sprintf("%v\n", s.Env))
	builder.WriteString(fmt.Sprintf("%v\n", s.TypeCheck))
	builder.WriteString(fmt.Sprintf("%d\n", s.MemorySize))
	builder.WriteString(fmt.Sprintf("%d\n", s.CallStackSize))
	builder.WriteString(fmt.Sprintf("%s\n", strings.Join(s.IncludePaths, ",")))
	builder.WriteString(fmt.Sprintf("%d\n", s.Optimization))
	builder.WriteString(fmt.Sprintf("%s\n", s.ProfilePath))
	builder.WriteString(fmt.Sprintf("%d\n", s.LogLevel))

	return builder.String()
}

// ---------------------------------------------------------------------------------------------------------------------
