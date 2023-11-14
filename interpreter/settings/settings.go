package settings

import (
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"os"
	"strconv"
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
}

// ---------------------------------------------------------------------------------------------------------------------

// TODO: add flags for memory and call_stack instead of reading env?
func NewSettings(
	mode string, env, typecheck bool, mem types.IntType, call_stack_Size int,
	include_paths utils.ArrayArgs, opt int, profile_fn string, loglevel string) *Settings {
	value, exists := os.LookupEnv("GORTH_VM_MEMORY")
	if exists {
		val_int, err := strconv.ParseInt(value, 10, 64)
		if err != nil || val_int <= 0 {
			logger.VmCrash(nil, "Incorrect value for env variable GORTH_VM_MEMORY: %s", value)
		}
		mem = val_int
	}

	value, exists = os.LookupEnv("GORTH_VM_CALL_STACK")
	if exists {
		val_int, err := strconv.Atoi(value)
		if err != nil || val_int <= 0 {
			logger.VmCrash(nil, "Incorrect value for env variable GORTH_VM_CALL_STACK: %s", value)
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
		logger.VmCrash(nil, "Incorrect value for log level: %s", loglevel)
	}

	return &Settings{
		Mode: mode, Env: env, TypeCheck: typecheck, MemorySize: mem, CallStackSize: call_stack_Size,
		IncludePaths: include_paths, Optimization: opt, ProfilePath: profile_fn, LogLevel: l,
	}
}

func (s *Settings) IsDebug() bool {
	return s.Mode == "debug"
}

// ---------------------------------------------------------------------------------------------------------------------
