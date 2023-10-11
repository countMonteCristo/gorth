package logger

type LogLevelType int

const (
	Fatal LogLevelType = iota
	Error
	RuntimeError
	Warning
	Info
	None
)

func LogLevelToStr(level LogLevelType) string {
	switch level {
	case Fatal:
		return "FATAL"
	case Error:
		return "ERROR"
	case RuntimeError:
		return "RUNTIME_ERROR"
	case Warning:
		return "WARNING"
	case Info:
		return "INFO"
	case None:
		return ""
	}
	panic("Unknown log message type")
}
