package vm

import (
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"os"
	"strconv"
)

// ---------------------------------------------------------------------------------------------------------------------

type VmSettings struct {
	Debug         bool // debug mode
	Env           bool // store env variables to memory
	TypeCheck     bool // do type checking before running the program
	MemorySize    types.IntType
	CallStackSize int
	IncludePaths  utils.ArrayArgs
	Optimization  int
}

// ---------------------------------------------------------------------------------------------------------------------

// TODO: add flags for memory and call_stack instead of reading env?
func NewSettings(
	debug, env, typecheck bool, mem types.IntType, call_stack_Size int, include_paths utils.ArrayArgs, opt int) *VmSettings {
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

	return &VmSettings{
		Debug: debug, Env: env, TypeCheck: typecheck, MemorySize: mem, CallStackSize: call_stack_Size,
		IncludePaths: include_paths, Optimization: opt,
	}
}

// ---------------------------------------------------------------------------------------------------------------------
