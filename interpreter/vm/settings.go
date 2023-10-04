package vm

import (
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"os"
	"strconv"
)

type Settings struct {
	Debug         bool // debug mode
	Env           bool // store env variables to memory
	MemorySize    types.IntType
	CallStackSize int
}

func NewSettings(debug, env bool, mem types.IntType, call_stack_Size int) *Settings {
	value, exists := os.LookupEnv("GORTH_VM_MEMORY")
	if exists {
		val_int, err := strconv.ParseInt(value, 10, 64)
		if err != nil || val_int <= 0 {
			logger.Crash(nil, "Incorrect value for env variable GORTH_VM_MEMORY: %s", value)
		}
		mem = val_int
	}

	value, exists = os.LookupEnv("GORTH_VM_CALL_STACK")
	if exists {
		val_int, err := strconv.Atoi(value)
		if err != nil || val_int <= 0 {
			logger.Crash(nil, "Incorrect value for env variable GORTH_VM_CALL_STACK: %s", value)
		}
		call_stack_Size = val_int
	}

	return &Settings{Debug: debug, Env: env, MemorySize: mem, CallStackSize: call_stack_Size}
}
