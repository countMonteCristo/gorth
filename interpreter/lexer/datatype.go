package lexer

import "Gorth/interpreter/utils"

type DataType int

const (
	DataTypeInt DataType = iota
	DataTypePtr
	DataTypeBool
)

var DataTypeName = map[DataType]string{
	DataTypeInt:  "int",
	DataTypePtr:  "ptr",
	DataTypeBool: "bool",
}

var WordToDataType = utils.RevMap(DataTypeName).(map[string]DataType)
