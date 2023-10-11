package logger

type ModuleType int

const (
	ModuleNone ModuleType = iota
	ModuleLexer
	ModuleCompiler
	ModuleTypeChecker
	ModuleVm
	ModuleDebugger
)

func ModuleToStr(module ModuleType) string {
	switch module {
	case ModuleNone:
		return "NONE"
	case ModuleLexer:
		return "LEXER"
	case ModuleCompiler:
		return "COMPILER"
	case ModuleTypeChecker:
		return "TYPE_CHECKER"
	case ModuleVm:
		return "VM"
	case ModuleDebugger:
		return "DEBUGGER"
	default:
		panic("Unknown module type")
	}
}
