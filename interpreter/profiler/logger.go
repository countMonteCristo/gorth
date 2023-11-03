package profiler

import (
	"Gorth/interpreter/types"
)

type Logger struct {
	Messages chan Message
	Stats    map[types.IntType]*OpTimings
}

func NewLogger() *Logger {
	return &Logger{
		Messages: make(chan Message),
		Stats:    make(map[int64]*OpTimings, 0),
	}
}

func (l *Logger) Run() {
	for {
		msg := <-l.Messages
		if msg.OpId == -1 {
			break
		}

		if _, exists := l.Stats[msg.OpId]; !exists {
			l.Stats[msg.OpId] = NewOpTimings()
		}
		l.Stats[msg.OpId].Timing = append(l.Stats[msg.OpId].Timing, msg.Time)
	}
}
