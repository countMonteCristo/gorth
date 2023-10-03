package utils

type LogLevelType int

const (
	Fatal LogLevelType = iota
	Error
	Warning
	Info
	None
)

func LogMsgTypeToStr(level LogLevelType) string {
	switch level {
	case Fatal:
		return "FATAL"
	case Error:
		return "ERROR"
	case Warning:
		return "WARNING"
	case Info:
		return "INFO"
	case None:
		return ""
	}
	panic("Unknown log message type")
}
