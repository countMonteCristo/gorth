package utils

// ---------------------------------------------------------------------------------------------------------------------

type Stack[T any] struct {
	Data []T
}

func NewStack[T any](x []T) *Stack[T] {
	s := &Stack[T]{}
	for _, i := range x {
		s.Push(i)
	}
	return s
}

// ---------------------------------------------------------------------------------------------------------------------

func (s *Stack[T]) Push(x T) {
	s.Data = append(s.Data, x)
}

func (s *Stack[T]) Pop() (x T) {
	if len(s.Data) > 0 {
		x = s.Data[len(s.Data)-1]
		s.Data = s.Data[:len(s.Data)-1]
	} else {
		panic("Stack underflow")
	}
	return
}

func (s *Stack[T]) Top() (x T) {
	if len(s.Data) > 0 {
		x = s.Data[len(s.Data)-1]
	} else {
		panic("Stack underflow")
	}
	return
}

// ---------------------------------------------------------------------------------------------------------------------

func (s *Stack[T]) Size() int {
	return len(s.Data)
}

func (s *Stack[T]) Empty() bool {
	return s.Size() == 0
}

// ---------------------------------------------------------------------------------------------------------------------

func (s *Stack[T]) Copy() *Stack[T] {
	n := &Stack[T]{Data: make([]T, s.Size())}
	copy(n.Data, s.Data)
	return n
}

// ---------------------------------------------------------------------------------------------------------------------

func (s *Stack[T]) Clear() {
	s.Data = make([]T, 0)
}

// ---------------------------------------------------------------------------------------------------------------------

func StackAsSlice[T any](s *Stack[T]) []T {
	result := make([]T, s.Size())
	copy(result, s.Data)
	return result
}

// ---------------------------------------------------------------------------------------------------------------------

func StacksAreEqual[T comparable](s, v *Stack[T]) bool {
	if s.Size() != v.Size() {
		return false
	}
	for i := range s.Data {
		if s.Data[i] != v.Data[i] {
			return false
		}
	}
	return true
}

// ---------------------------------------------------------------------------------------------------------------------
