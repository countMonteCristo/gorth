package types

import "Gorth/interpreter/utils"

type BoolType int

const (
	BoolTrue  BoolType = iota
	BoolFalse BoolType = iota
)

var BoolName = map[BoolType]string{
	BoolTrue:  "true",
	BoolFalse: "false",
}

var WordToBool = utils.RevMap(BoolName).(map[string]BoolType)
