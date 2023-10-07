package utils

import (
	"fmt"
)

type Stack struct {
	Data []interface{}
}

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

func (s *Stack) Size() int {
	return len(s.Data)
}

func (s *Stack) Empty() bool {
	return s.Size() == 0
}
