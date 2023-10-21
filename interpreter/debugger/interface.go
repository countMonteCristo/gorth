package debugger

import (
	"Gorth/interpreter/types"
	"Gorth/interpreter/vm"
	"fmt"
)

// ---------------------------------------------------------------------------------------------------------------------

type DebugInterface struct {
	Commands    chan DebugCommand
	Response    chan DebugCommandResponse
	BreakPoints map[types.IntType]bool
}

func NewDebugInterface() *DebugInterface {
	return &DebugInterface{
		Commands: make(chan DebugCommand), Response: make(chan DebugCommandResponse),
		BreakPoints: make(map[types.IntType]bool),
	}
}

// ---------------------------------------------------------------------------------------------------------------------

func (di *DebugInterface) PrintHelp() {
	fmt.Println("Availavle commands:")
	fmt.Println(" * `n` [`count`]       - process at most `count` instructions (by default `count`=1)")
	fmt.Println(" * `c`                 - continue (process all instructions to the break point or to the end)")
	fmt.Println(" * `u`                 - go out of current function")
	fmt.Println(" * `bs` `a1 a2 .. ak`  - set break points for functions or addresses")
	fmt.Println(" * `bl`                - list all break points")
	fmt.Println(" * `br` `a1 a2 .. ak`  - remove break points from functions or addresses")
	fmt.Println(" * `t`                 - print current token")
	fmt.Println(" * `o` [`ctx`]         - print current operation (+-`ctx` operations, by default `ctx`=0)")
	fmt.Println(" * `ol`                - print operations list")
	fmt.Println(" * `s`                 - print current stack state")
	fmt.Println(" * `m`                 - print current memory state")
	fmt.Println(" * `mo` `addr` `size`  - print memory chunk of size `size` at address `addr`")
	fmt.Println(" * `p` `n1 n2 .. nk`   - print consts, allocs or functions")
	fmt.Println(" * `e` [`type`]        - print current environment (consts, allocs), `type` could be [`all`, `local`, `global`], by default `type`=`all`")
	fmt.Println(" * `h[elp]`            - print help")
	fmt.Println(" * `r`                 - restart script")
	fmt.Println(" * `q`                 - exit debugger")
}

// ---------------------------------------------------------------------------------------------------------------------

func (di *DebugInterface) Communicate(cmd *DebugCommand) DebugCommandResponse {
	if cmd.Type == DebugCmdHelp {
		di.PrintHelp()
	}
	di.Commands <- *cmd
	return <-di.Response
}

// ---------------------------------------------------------------------------------------------------------------------

func (di *DebugInterface) SendOK() {
	di.Response <- DebugCommandResponse{Status: DebugCommandStatusOK}
}
func (di *DebugInterface) SendFailed(msg string) {
	di.Response <- DebugCommandResponse{
		Status: DebugCommandStatusFailed, Msg: msg,
	}
}

// ---------------------------------------------------------------------------------------------------------------------

func (di *DebugInterface) IsBreakpoint(ctx *vm.RunTimeContext) (types.IntType, bool) {
	_, exists := di.BreakPoints[ctx.Addr]
	if exists {
		return ctx.Addr, true
	}
	return -1, false
}

// ---------------------------------------------------------------------------------------------------------------------

func (di *DebugInterface) PrintOpsList(start, finish types.IntType, ops *[]vm.Op, ctx *vm.RunTimeContext) {
	path_column := make([]string, 0)
	markers_column := make([]string, 0)
	cmd_column := make([]string, 0)
	max_column_width := 0
	for addr := start; addr < ctx.OpsCount && addr <= finish; addr++ {
		if addr < 0 {
			continue
		}
		marker := " "
		if addr == ctx.Addr {
			marker = "*"
		}
		bp := " "
		_, exists := di.BreakPoints[addr]
		if exists {
			bp = "b"
		}
		op := (*ops)[addr]

		path := fmt.Sprintf("%s:%d:%d", op.OpToken.Loc.Filepath, op.OpToken.Loc.Line+1, op.OpToken.Loc.Column+1)
		if len(path) > max_column_width {
			max_column_width = len(path)
		}

		// fmt.Println(addr)

		path_column = append(path_column, path)
		markers_column = append(markers_column, fmt.Sprintf("%s%s", bp, marker))
		cmd_column = append(cmd_column, op.Str(addr))
	}

	for i, path := range path_column {
		fmt.Printf("%-*s %s%s\n", max_column_width, path, markers_column[i], cmd_column[i])
	}
}

// ---------------------------------------------------------------------------------------------------------------------
