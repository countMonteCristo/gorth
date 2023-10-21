package types

import (
	"Gorth/interpreter/utils"
)

// ---------------------------------------------------------------------------------------------------------------------

type IntType = int64
type BoolType = IntType

// ---------------------------------------------------------------------------------------------------------------------

const (
	BoolTrue  BoolType = 1
	BoolFalse BoolType = 0
)

var BoolType2Str = map[BoolType]string{
	BoolTrue:  "true",
	BoolFalse: "false",
}

var Str2Bool = utils.RevMap(BoolType2Str).(map[string]BoolType)

// ---------------------------------------------------------------------------------------------------------------------
