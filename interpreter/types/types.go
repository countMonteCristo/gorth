package types

import "Gorth/interpreter/utils"

type BoolType = bool

const (
	BoolTrue  BoolType = true
	BoolFalse BoolType = false
)

var BoolName = map[BoolType]string{
	BoolTrue:  "true",
	BoolFalse: "false",
}

var WordToBool = utils.RevMap(BoolName).(map[string]BoolType)

type IntType = int64
