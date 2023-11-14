package datatypes

import (
	"Gorth/interpreter/utils"
	"fmt"
	"strings"
)

// ---------------------------------------------------------------------------------------------------------------------

type TypeStack = utils.Stack[DataType]

// ---------------------------------------------------------------------------------------------------------------------

type DataType int
type DataTypes []DataType

const (
	DataTypeAny DataType = iota
	DataTypeInt
	DataTypePtr
	DataTypeBool
	DataTypeFptr
)

var DataType2Str = map[DataType]string{
	DataTypeAny:  "any",
	DataTypeInt:  "int",
	DataTypePtr:  "ptr",
	DataTypeBool: "bool",
	DataTypeFptr: "fptr",
}

// ---------------------------------------------------------------------------------------------------------------------

func (t DataType) String() string {
	return DataType2Str[t]
}

func (types DataTypes) String() string {
	res := utils.MapF(types, func(t DataType) string { return DataType2Str[t] })
	return fmt.Sprintf("[%s]", strings.Join(res, ", "))
}

// ---------------------------------------------------------------------------------------------------------------------

var Str2DataType = utils.RevMap(DataType2Str).(map[string]DataType)

// ---------------------------------------------------------------------------------------------------------------------
