package lexer

import (
	"Gorth/interpreter/utils"
	"fmt"
	"strings"
)

type DataType int
type DataTypes []DataType

// TODO: add `any` datatype?
const (
	DataTypeAny DataType = iota
	DataTypeInt
	DataTypePtr
	DataTypeBool
)

var DataTypeName = map[DataType]string{
	DataTypeAny: "any",
	DataTypeInt:  "int",
	DataTypePtr:  "ptr",
	DataTypeBool: "bool",
}

func (t DataType) String() string {
	return DataTypeName[t]
}

func (types DataTypes) String() string {
	res := utils.MapF(types, func(t DataType) string { return DataTypeName[t] })
	return fmt.Sprintf("[%s]", strings.Join(res, ", "))
}

var WordToDataType = utils.RevMap(DataTypeName).(map[string]DataType)
