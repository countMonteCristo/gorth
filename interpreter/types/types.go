package types

import (
	"Gorth/interpreter/utils"
)

type IntType = int64
type BoolType = IntType

const (
	BoolTrue  BoolType = 1
	BoolFalse BoolType = 0
)

var BoolName = map[BoolType]string{
	BoolTrue:  "true",
	BoolFalse: "false",
}

var WordToBool = utils.RevMap(BoolName).(map[string]BoolType)
