package utils

import "fmt"

// ---------------------------------------------------------------------------------------------------------------------

type Location struct {
	Filepath string
	Line     int
	Column   int
}

func (l *Location) String() string {
	return fmt.Sprintf("%s:%d:%d", l.Filepath, l.Line+1, l.Column+1)
}

// ---------------------------------------------------------------------------------------------------------------------
