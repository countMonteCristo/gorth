package utils

import (
	"fmt"
)

// ---------------------------------------------------------------------------------------------------------------------

type Stack struct {
	Data []interface{}
}

func NewStack[T any](x []T) *Stack {
	s := &Stack{}
	for _, i := range x {
		s.Push(i)
	}
	return s
}

// ---------------------------------------------------------------------------------------------------------------------

func (s *Stack) Push(x interface{}) {
	s.Data = append(s.Data, x)
}

func (s *Stack) Pop() (x interface{}) {
	if len(s.Data) > 0 {
		x = s.Data[len(s.Data)-1]
		s.Data = s.Data[:len(s.Data)-1]
	} else {
		fmt.Println("Stack underflow")
		Exit(1)
	}
	// fmt.Printf("INFO: stack after pop: %v\n", s.Data)
	return
}

func (s *Stack) Top() (x interface{}) {
	if len(s.Data) > 0 {
		x = s.Data[len(s.Data)-1]
	} else {
		fmt.Println("Stack underflow")
		Exit(1)
	}
	return
}

// ---------------------------------------------------------------------------------------------------------------------

func (s *Stack) Size() int {
	return len(s.Data)
}

func (s *Stack) Empty() bool {
	return s.Size() == 0
}

// ---------------------------------------------------------------------------------------------------------------------

func (s *Stack) Copy() *Stack {
	n := &Stack{Data: make([]interface{}, s.Size())}
	copy(n.Data, s.Data)
	return n
}

// ---------------------------------------------------------------------------------------------------------------------

func (s *Stack) Clear() {
	s.Data = make([]interface{}, 0)
}

// ---------------------------------------------------------------------------------------------------------------------

func StackAsSlice[T any](s *Stack) []T {
	result := make([]T, s.Size())
	for i, item := range s.Data {
		result[i] = item.(T)
	}
	return result
}

// ---------------------------------------------------------------------------------------------------------------------

func StacksAreEqual[T comparable](s, v *Stack) bool {
	if s.Size() != v.Size() {
		return false
	}
	for i := range s.Data {
		if s.Data[i].(T) != v.Data[i].(T) {
			return false
		}
	}
	return true
}

// ---------------------------------------------------------------------------------------------------------------------
