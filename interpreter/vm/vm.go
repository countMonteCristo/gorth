package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
	"sort"
	"unsafe"

	"golang.org/x/sys/unix"
)

type VM struct {
	Ctx            Context
	RecursionLimit int
}

func InitVM() *VM {
	vm := VM{
		Ctx:            *InitContext(640 * 1024), // 640k is enough for everybody, huh?
		RecursionLimit: 1000,
	}

	return &vm
}

// Named block starts with a keyword (const|alloc) and followed by block name
// Every named block ends with `end` keyword and may contain only consts and instrinsics
func (vm *VM) parse_named_block(token *lexer.Token, tokens *[]lexer.Token, typ string, scope *Scope) (name_token lexer.Token, const_value types.IntType) {
	if len(*tokens) == 0 {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name, but got nothing", typ))
	}

	name_token, *tokens = (*tokens)[0], (*tokens)[1:]

	if name_token.Typ != lexer.TokenWord {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name to be a word, but got `%s`", typ, name_token.Text))
	}
	defined_token, exists := scope.Names[name_token.Text]
	if exists {
		lexer.CompilerInfo(&name_token.Loc, fmt.Sprintf("Redefinition of word `%s`", name_token.Text))
		lexer.CompilerInfo(&defined_token.Loc, "Previously defined here")
		utils.Exit(1)
	}

	defined_token, exists = vm.Ctx.GlobalScope().Names[name_token.Text]
	if exists {
		_, func_exists := vm.Ctx.Funcs[name_token.Text]
		if func_exists {
			lexer.CompilerInfo(&name_token.Loc, fmt.Sprintf("Redefinition of function `%s`", name_token.Text))
			lexer.CompilerInfo(&defined_token.Loc, "Previously defined here")
			utils.Exit(1)
		}

		lexer.CompilerInfo(&name_token.Loc, fmt.Sprintf("Redefinition of word `%s`", name_token.Text))
		lexer.CompilerInfo(&defined_token.Loc, "Previously defined here")
	}

	scope.Names[name_token.Text] = name_token

	const_block := make([]lexer.Token, 0)
	for {
		if len(*tokens) == 0 {
			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unexpected end while processing `%s` block", typ))
		}
		var btok lexer.Token
		btok, *tokens = (*tokens)[0], (*tokens)[1:]

		if btok.Typ == lexer.TokenKeyword && btok.Value.(lexer.KeywordType) == lexer.KeywordEnd {
			break
		}

		const_block = append(const_block, btok)
	}

	const_value = vm.const_eval(&name_token, &const_block, scope)
	return
}

func (vm *VM) const_eval(name_token *lexer.Token, tokens *[]lexer.Token, scope *Scope) (value types.IntType) {
	const_stack := &utils.Stack{}
	for _, token := range *tokens {
		switch token.Typ {
		case lexer.TokenInt:
			const_stack.Push(token.Value.(types.IntType))
		case lexer.TokenWord:
			intrinsic, exists := lexer.WordToIntrinsic[token.Text]
			if exists {
				switch intrinsic {
				case lexer.IntrinsicPlus, lexer.IntrinsicMinus, lexer.IntrinsicMul:
					b := const_stack.Pop().(types.IntType)
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(SafeArithmeticFunctions[intrinsic](a, b))
				case lexer.IntrinsicDiv:
					b := const_stack.Pop().(types.IntType)
					if b == 0 {
						lexer.CompilerFatal(&token.Loc, "Division by zero")
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a / b)
				case lexer.IntrinsicMod:
					b := const_stack.Pop().(types.IntType)
					if b == 0 {
						lexer.CompilerFatal(&token.Loc, "Division by zero")
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a % b)
				case lexer.IntrinsicShl:
					b := const_stack.Pop().(types.IntType)
					if b < 0 {
						lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Negative shift amount in `<<`: %d", b))
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a << b)
				case lexer.IntrinsicShr:
					b := const_stack.Pop().(types.IntType)
					if b < 0 {
						lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Negative shift amount in `>>`: %d", b))
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a >> b)
				case lexer.IntrinsicOffset:
					off := const_stack.Pop().(types.IntType)
					const_stack.Push(vm.Ctx.Offset)
					vm.Ctx.Offset += off
				case lexer.IntrinsicReset:
					const_stack.Push(vm.Ctx.Offset)
					vm.Ctx.Offset = 0
				default:
					lexer.CompilerFatal(
						&token.Loc,
						fmt.Sprintf(
							"Unexpected intrinsic in const-block compile-time "+
								"evaluation: `%s`. Supported: [+, -, *, /, %%, >>, <<, offset, reset]",
							token.Text,
						),
					)
				}
				continue
			}

			val, scope := vm.Ctx.GetConst(token.Text, scope.ScopeName)
			if scope != ScopeUnknown {
				const_stack.Push(val)
				continue
			}
			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unsupported word in compile-time const-block evaluation: `%s`", token.Text))
		default:
			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unsupported token in compile-time const-block evaluation: `%s`", token.Text))
		}
	}

	if const_stack.Size() > 1 {
		lexer.CompilerFatal(&name_token.Loc, "Unhandled data in compile-time const-block evaluation stack")
	}

	value, _ = const_stack.Pop().(types.IntType)
	return
}

func (vm *VM) parse_func_def(token *lexer.Token, tokens *[]lexer.Token) (name_token lexer.Token, func_name string) {
	if len(*tokens) == 0 {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name, but got nothing", token.Text))
	}

	name_token, *tokens = (*tokens)[0], (*tokens)[1:]

	if name_token.Typ != lexer.TokenWord {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name to be a word, but got `%s`", token.Text, name_token.Text))
	}

	defined_token, exists := vm.Ctx.GlobalScope().Names[name_token.Text]
	if exists {
		lexer.CompilerInfo(&name_token.Loc, fmt.Sprintf("Redefinition of word `%s` in function definition", name_token.Text))
		lexer.CompilerInfo(&defined_token.Loc, "Previously defined here")
		utils.Exit(1)
	}

	func_name = name_token.Text
	vm.Ctx.GlobalScope().Names[func_name] = name_token

	var do_token lexer.Token
	do_token, *tokens = (*tokens)[0], (*tokens)[1:]

	if do_token.Typ != lexer.TokenKeyword {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `do` to start the function name, but got `%s`", token.Text))
	}
	if do_token.Value.(lexer.KeywordType) != lexer.KeywordDo {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected keyword `do` to start the function name, but got `%s`", token.Text))
	}

	return
}

func (vm *VM) preprocess_string_literals(tokens *[]lexer.Token) {
	address := types.IntType(1)
	for _, token := range *tokens {
		if token.Typ == lexer.TokenString {
			literal := token.Value.(string)
			_, exists := vm.Ctx.Memory.StringsMap[literal]
			if !exists {
				vm.Ctx.Memory.StringsMap[literal] = address
				address += types.IntType(len(literal) + 1) // save literals as null-terminated strings
			}
		}
	}

	vm.Ctx.Memory.StringsRegion = MemoryRegion{
		Start: 1,
		Size:  address - 1,
		Ptr:   address,
	}
}

func (vm *VM) Compile(fn string, tokens []lexer.Token, args []string) (ops []Op) {
	// assert(lexer.TokenCount == 6, "Unhandled Token in compile()")

	blocks := &utils.Stack{}

	current_scope_name := GlobalScopeName

	vm.preprocess_string_literals(&tokens)
	vm.Ctx.Memory.Prepare(args)

	var token lexer.Token
	for len(tokens) > 0 {

		token, tokens = tokens[0], tokens[1:]
		// fmt.Printf("Process token %v\n", token)

		switch token.Typ {
		case lexer.TokenInt:
			ops = append(ops, Op{
				Typ:     OpPushInt,
				Operand: token.Value.(types.IntType),
				OpToken: token,
			})
		case lexer.TokenString:
			literal := token.Value.(string)
			literal_addr := vm.Ctx.Memory.StringsMap[literal]
			ops = append(ops, Op{
				Typ:     OpPushInt,
				Operand: literal_addr,
				OpToken: token,
			})
			ops = append(ops, Op{
				Typ:     OpPushInt,
				Operand: types.IntType(len(literal)),
				OpToken: token,
			})
		case lexer.TokenChar:
			ops = append(ops, Op{
				Typ:     OpPushInt,
				Operand: token.Value.(types.IntType),
				OpToken: token,
			})
		case lexer.TokenBool:
			ops = append(ops, Op{
				Typ:     OpPushBool,
				Operand: token.Value.(types.BoolType),
				OpToken: token,
			})
		case lexer.TokenWord:
			name := token.Text

			intrinsic, exists := lexer.WordToIntrinsic[name]
			if exists {
				ops = append(ops, Op{
					Typ:     OpIntrinsic,
					Operand: intrinsic,
					OpToken: token,
				})
				continue
			}

			val, exists := vm.Ctx.GetLocalConst(name, current_scope_name)
			if exists {
				ops = append(ops, Op{
					Typ:     OpPushInt,
					Operand: val,
					OpToken: token,
				})
				continue
			}

			_, exists = vm.Ctx.GetLocalAlloc(name, current_scope_name)
			if exists {
				ops = append(ops, Op{
					Typ:     OpPushAlloc,
					Operand: name,
					OpToken: token,
				})
				continue
			}

			val, exists = vm.Ctx.GetGlobalConst(name)
			if exists {
				ops = append(ops, Op{
					Typ:     OpPushInt,
					Operand: val,
					OpToken: token,
				})
				continue
			}

			_, exists = vm.Ctx.GetGlobalAlloc(name)
			if exists {
				ops = append(ops, Op{
					Typ:     OpPushAlloc,
					Operand: name,
					OpToken: token,
				})
				continue
			}

			func_addr, exists := vm.Ctx.Funcs[name]
			if exists {
				ops_count := types.IntType(len(ops))
				ops = append(ops, Op{
					Typ:     OpCall,
					Operand: func_addr - ops_count,
					OpToken: token,
				})
				continue
			}

			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unknown word: `%s`", token.Text))

		case lexer.TokenKeyword:
			kw_type := token.Value.(lexer.KeywordType)
			op := Op{OpToken: token}
			len_ops := types.IntType(len(ops))

			scope := vm.Ctx.Scopes[current_scope_name]

			switch kw_type {
			case lexer.KeywordIf:
				op.Typ = OpIf
				blocks.Push(Block{Addr: len_ops, Tok: token})
				ops = append(ops, op)
			case lexer.KeywordElse:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `end` found")
				}
				block := blocks.Pop().(Block)
				if block.Tok.Typ != lexer.TokenKeyword {
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but not `%s`. Probably bug in lex()", block.Tok.Text))
				}
				block_start_kw := block.Tok.Value.(lexer.KeywordType)
				switch block_start_kw {
				case lexer.KeywordIf:
					ops[block.Addr].Operand = len_ops - block.Addr + 1
				case lexer.KeywordElse, lexer.KeywordEnd, lexer.KeywordWhile:
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("`else` may only come after `if` block, but got `%s`", block.Tok.Text))
				default:
					lexer.CompilerFatal(&token.Loc, "Unhandled block start processing in vm.Compile() at KeywordElse")
				}

				op.Typ = OpElse
				blocks.Push(Block{Addr: len_ops, Tok: token})
				ops = append(ops, op)
			case lexer.KeywordEnd:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `end` found")
				}
				block := blocks.Pop().(Block)
				if block.Tok.Typ != lexer.TokenKeyword {
					lexer.CompilerFatal(
						&token.Loc, fmt.Sprintf(
							"Only keywords may form blocks, but not `%s`. Probably bug in lex()",
							block.Tok.Text,
						),
					)
				}
				block_start_kw := block.Tok.Value.(lexer.KeywordType)
				op.Operand = types.IntType(1)
				switch block_start_kw {
				case lexer.KeywordIf:
					ops[block.Addr].Operand = len_ops - block.Addr + 1
					op.Typ = OpEnd
					ops = append(ops, op)
				case lexer.KeywordElse:
					ops[block.Addr].Operand = len_ops - block.Addr + 1
					op.Typ = OpEnd
					ops = append(ops, op)
				case lexer.KeywordWhile:
					lexer.CompilerFatal(&block.Tok.Loc, "`while` block must contain `do` before `end`")
				case lexer.KeywordDo:
					for _, jump := range block.Jumps {
						switch jump.Keyword {
						case lexer.KeywordBreak:
							ops[jump.Addr].Operand = len_ops - jump.Addr + 1
						case lexer.KeywordContinue:
							ops[jump.Addr].Operand = ops[block.Addr].Operand.(types.IntType) + (block.Addr - jump.Addr)
						default:
							lexer.CompilerFatal(&block.Tok.Loc, fmt.Sprintf("Unhandled jump-keyword: `%s`", lexer.KeywordName[jump.Keyword]))
						}
					}
					op.Operand = ops[block.Addr].Operand.(types.IntType) + (block.Addr - len_ops)
					ops[block.Addr].Operand = len_ops - block.Addr + 1
					op.Typ = OpEnd
					ops = append(ops, op)
				case lexer.KeywordEnd:
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("`end` may only close `if-else` or `while-do` blocks, but got `%s`", block.Tok.Text))
				case lexer.KeywordBreak:
					lexer.CompilerFatal(&block.Tok.Loc, "`break` keyword shouldn't be in blocks stack")
				case lexer.KeywordFunc:
					func_end_op := Op{
						OpToken: token, Typ: OpFuncEnd, Operand: current_scope_name,
					}
					current_scope_name = GlobalScopeName // we do not allow nested function definitions so it is ok
					ops = append(ops, func_end_op)
				default:
					lexer.CompilerFatal(&token.Loc, "Unhandled block start processing")
				}
			case lexer.KeywordWhile:
				op.Typ = OpWhile
				blocks.Push(Block{Addr: len_ops, Tok: token})
				ops = append(ops, op)
			case lexer.KeywordDo:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `do` found")
				}
				block := blocks.Pop().(Block)
				if block.Tok.Typ != lexer.TokenKeyword {
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but got `%s`", block.Tok.Text))
				}
				if block.Tok.Value.(lexer.KeywordType) != lexer.KeywordWhile {
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("`do` may come only inside `while` block, but not `%s`", block.Tok.Text))
				}
				op.Typ = OpDo
				op.Operand = block.Addr - len_ops // save relative address of `while`
				blocks.Push(Block{Addr: len_ops, Tok: token})
				ops = append(ops, op)
			case lexer.KeywordBreak:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `break` found")
				}
				var i int

				for i = len(blocks.Data) - 1; i >= 0; i-- {
					cur_block := blocks.Data[i].(Block)

					if cur_block.Tok.Typ == lexer.TokenKeyword && cur_block.Tok.Value.(lexer.KeywordType) == lexer.KeywordDo {
						cur_block.Jumps = append(cur_block.Jumps, Jump{Keyword: lexer.KeywordBreak, Addr: len_ops})
						blocks.Data[i] = cur_block
						break
					}
				}
				if i < 0 {
					lexer.CompilerFatal(&token.Loc, "Break should be inside while-loop, but it doesn't")
				}

				op.Typ = OpBreak
				ops = append(ops, op)
			case lexer.KeywordContinue:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `break` found")
				}
				var i int

				for i = len(blocks.Data) - 1; i >= 0; i-- {
					cur_block := blocks.Data[i].(Block)

					if cur_block.Tok.Typ == lexer.TokenKeyword && cur_block.Tok.Value.(lexer.KeywordType) == lexer.KeywordDo {
						cur_block.Jumps = append(cur_block.Jumps, Jump{Keyword: lexer.KeywordContinue, Addr: len_ops})
						blocks.Data[i] = cur_block
						break
					}
				}
				if i < 0 {
					lexer.CompilerFatal(&token.Loc, "Break should be inside while-loop, but it doesn't")
				}

				op.Typ = OpContinue
				ops = append(ops, op)

			case lexer.KeywordConst:
				tok, const_value := vm.parse_named_block(&token, &tokens, token.Text, scope)
				scope.Consts[tok.Text] = const_value
			case lexer.KeywordAlloc:
				tok, alloc_size := vm.parse_named_block(&token, &tokens, token.Text, scope)
				if alloc_size < 0 {
					lexer.CompilerFatal(&tok.Loc, fmt.Sprintf("Negative size for `alloc` block: %d", alloc_size))
				}
				scope.Allocs[tok.Text] = Allocation{
					Offset: scope.MemSize, Size: alloc_size,
				}
				scope.MemSize += alloc_size

			case lexer.KeywordFunc:
				if current_scope_name != GlobalScopeName {
					lexer.CompilerFatal(
						&token.Loc,
						fmt.Sprintf("Cannot define functions inside a function %s", current_scope_name),
					)
				}
				func_token, func_name := vm.parse_func_def(&token, &tokens)
				current_scope_name = func_name

				new_scope := NewScope(func_name)
				new_scope.Names[func_name] = func_token
				vm.Ctx.Scopes[func_name] = new_scope

				vm.Ctx.Funcs[func_name] = len_ops
				blocks.Push(Block{Addr: len_ops, Tok: token})

				op.Typ = OpFuncBegin
				op.Operand = func_name
				ops = append(ops, op)

			case lexer.KeywordInclude:
				lexer.CompilerFatal(&token.Loc, "Include keyword should not appear in here, probably there is a bug in a lexer")
			default:
				lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unhandled KewordType handling: `%s`", token.Text))
			}
		default:
			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unhandled token: `%s`\n", token.Text))
		}
	}

	if len(blocks.Data) > 0 {
		top := blocks.Data[len(blocks.Data)-1].(Block)
		lexer.CompilerFatal(&top.Tok.Loc, fmt.Sprintf("Unclosed `%s`-block", top.Tok.Text))
	}

	_, exists := vm.Ctx.Funcs["main"]
	if !exists {
		fmt.Printf("[ERROR] No entry point found (function `main` was not defined at %s)\n", fn)
		utils.Exit(1)
	}

	return
}

type ExitCodeType struct {
	Code types.IntType
	Msg  string
}

type ScriptContext struct {
	Stack       utils.Stack
	ReturnStack utils.Stack
	FuncStack   utils.Stack
	Addr        types.IntType
	Args        []string
	OpsCount    types.IntType
	ExitCode    ExitCodeType
}

func NewScriptContext(len_ops types.IntType, args []string) *ScriptContext {
	sc := &ScriptContext{
		Stack: utils.Stack{}, ReturnStack: utils.Stack{},
		Addr: 0, Args: args, OpsCount: len_ops, FuncStack: utils.Stack{},
		ExitCode: ExitCodeType{Code: 0},
	}
	sc.ReturnStack.Push(len_ops)
	sc.FuncStack.Push(GlobalScopeName)
	return sc
}

func (sc *ScriptContext) CurrentScopeName() string {
	return sc.FuncStack.Top().(string)
}

func (sc *ScriptContext) GetExitCode(ops []Op) ExitCodeType {
	switch {
	case sc.Stack.Size() > 1:
		sc.ExitCode.Code = types.IntType(1)
		sc.ExitCode.Msg = fmt.Sprintf("Multiple values left in stack after script exit: %v", sc.Stack.Data)
	case sc.Stack.Size() == 0:
		sc.ExitCode.Code = types.IntType(2)
		sc.ExitCode.Msg = "Empty stack after script exit"
	default:
		sc.ExitCode.Code = sc.Stack.Pop().(types.IntType)
	}
	return sc.ExitCode
}

func (vm *VM) ProcessSyscall(sc *ScriptContext) {
	syscall_id := sc.Stack.Pop().(types.IntType)
	switch syscall_id {
	case unix.SYS_OPEN:
		mode := sc.Stack.Pop().(types.IntType)
		flags := sc.Stack.Pop().(types.IntType)
		ptr := sc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_OPEN, uintptr(unsafe.Pointer(&vm.Ctx.Memory.Data[ptr])), uintptr(flags), uintptr(mode),
		)
		sc.Stack.Push(types.IntType(r1))
		sc.Stack.Push(types.IntType(err))
	case unix.SYS_READ:
		count := sc.Stack.Pop().(types.IntType)
		ptr := sc.Stack.Pop().(types.IntType)
		fd := sc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_READ, uintptr(fd), uintptr(unsafe.Pointer(&vm.Ctx.Memory.Data[ptr])), uintptr(count),
		)
		sc.Stack.Push(types.IntType(r1))
		sc.Stack.Push(types.IntType(err))
	case unix.SYS_CLOSE:
		fd := sc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_CLOSE, uintptr(fd), 0, 0,
		)
		sc.Stack.Push(types.IntType(r1))
		sc.Stack.Push(types.IntType(err))
	case unix.SYS_WRITE:
		count := sc.Stack.Pop().(types.IntType)
		ptr := sc.Stack.Pop().(types.IntType)
		fd := sc.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_WRITE, uintptr(fd), uintptr(unsafe.Pointer(&vm.Ctx.Memory.Data[ptr])), uintptr(count),
		)
		sc.Stack.Push(types.IntType(r1))
		sc.Stack.Push(types.IntType(err))
	default:
		panic(fmt.Sprintf("Syscall #%d is not implemented yet\n", syscall_id))
	}
}

func (vm *VM) Step(ops []Op, sc *ScriptContext) {
	op := ops[sc.Addr]
	switch op.Typ {
	case OpPushInt, OpPushBool:
		sc.Stack.Push(op.Operand)
		sc.Addr++
	case OpIf, OpDo:
		top := sc.Stack.Pop().(types.BoolType)
		if top {
			sc.Addr++
		} else {
			sc.Addr += op.Operand.(types.IntType)
		}
	case OpElse, OpEnd, OpBreak, OpContinue:
		sc.Addr += op.Operand.(types.IntType)
	case OpWhile:
		sc.Addr++
	case OpIntrinsic:
		intrinsic := op.Operand.(lexer.IntrinsicType)
		switch intrinsic {
		case lexer.IntrinsicPlus, lexer.IntrinsicMinus, lexer.IntrinsicMul, lexer.IntrinsicBitAnd, lexer.IntrinsicBitOr, lexer.IntrinsicBitXor:
			b := sc.Stack.Pop().(types.IntType)
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(SafeArithmeticFunctions[intrinsic](a, b))
		case lexer.IntrinsicDiv:
			b := sc.Stack.Pop().(types.IntType)
			if b == 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, "Division by zero")
			}
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(a / b)
		case lexer.IntrinsicMod:
			b := sc.Stack.Pop().(types.IntType)
			if b == 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, "Division by zero")
			}
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(a % b)
		case lexer.IntrinsicShl:
			b := sc.Stack.Pop().(types.IntType)
			if b < 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Negative shift amount in `<<`: %d", b))
			}
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(a << b)
		case lexer.IntrinsicShr:
			b := sc.Stack.Pop().(types.IntType)
			if b < 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Negative shift amount in `>>`: %d", b))
			}
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(a >> b)
		case lexer.IntrinsicBitNot:
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(^a)
		case lexer.IntrinsicLogicalAnd, lexer.IntrinsicLogicalOr:
			b := sc.Stack.Pop().(types.BoolType)
			a := sc.Stack.Pop().(types.BoolType)
			sc.Stack.Push(LogicalFunctions[intrinsic](a, b))
		case lexer.IntrinsicLogicalNot:
			x := sc.Stack.Pop()
			sc.Stack.Push(!x.(types.BoolType))
		case lexer.IntrinsicDup:
			x := sc.Stack.Top()
			sc.Stack.Push(x)
		case lexer.IntrinsicSwap:
			b := sc.Stack.Pop()
			a := sc.Stack.Pop()
			sc.Stack.Push(b)
			sc.Stack.Push(a)
		case lexer.IntrinsicDrop:
			sc.Stack.Pop()
		case lexer.IntrinsicOver:
			x := sc.Stack.Data[len(sc.Stack.Data)-2]
			sc.Stack.Push(x)
		case lexer.IntrinsicRot:
			c := sc.Stack.Pop()
			b := sc.Stack.Pop()
			a := sc.Stack.Pop()
			sc.Stack.Push(b)
			sc.Stack.Push(c)
			sc.Stack.Push(a)
		case lexer.IntrinsicEq, lexer.IntrinsicNe, lexer.IntrinsicLe, lexer.IntrinsicGe, lexer.IntrinsicLt, lexer.IntrinsicGt:
			b := sc.Stack.Pop().(types.IntType)
			a := sc.Stack.Pop().(types.IntType)
			sc.Stack.Push(ComparableFunctions[intrinsic](a, b))
		case lexer.IntrinsicPuti:
			x := sc.Stack.Pop()
			fmt.Print(x)
		case lexer.IntrinsicDebug:
			fmt.Printf(
				"\tMem: %v\tStack: %v\n",
				vm.Ctx.Memory.Data[vm.Ctx.Memory.OperativeMemRegion.Start:vm.Ctx.Memory.OperativeMemRegion.Ptr],
				sc.Stack.Data,
			)
		case lexer.IntrinsicLoad8, lexer.IntrinsicLoad16, lexer.IntrinsicLoad32, lexer.IntrinsicLoad64:
			ptr := sc.Stack.Pop().(types.IntType)
			val := vm.Ctx.Memory.LoadFromMem(ptr, LoadSizes[intrinsic])
			sc.Stack.Push(val)
		case lexer.IntrinsicStore8, lexer.IntrinsicStore16, lexer.IntrinsicStore32, lexer.IntrinsicStore64:
			ptr := sc.Stack.Pop().(types.IntType)
			x := sc.Stack.Pop().(types.IntType)
			vm.Ctx.Memory.StoreToMem(ptr, x, StoreSizes[intrinsic])

		case lexer.IntrinsicArgc:
			sc.Stack.Push(types.IntType(len(sc.Args)))
		case lexer.IntrinsicArgv:
			sc.Stack.Push(vm.Ctx.Memory.Argv)
		case lexer.IntrinsicSyscall:
			vm.ProcessSyscall(sc)
		default:
			lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled intrinsic: `%s`", op.OpToken.Text))
		}
		sc.Addr++
	case OpCall:
		if sc.ReturnStack.Size() >= vm.RecursionLimit {
			lexer.RuntimeFatal(&op.OpToken.Loc, "Recursion limit exceeded")
		}
		sc.ReturnStack.Push(sc.Addr)
		sc.Addr += op.Operand.(types.IntType)
	case OpFuncBegin:
		sc.Addr++
		sc.FuncStack.Push(op.Operand.(string))
		vm.Ctx.Memory.OperativeMemRegion.Ptr += vm.Ctx.Scopes[sc.CurrentScopeName()].MemSize
	case OpFuncEnd:
		if sc.ReturnStack.Size() == 0 {
			lexer.RuntimeFatal(&op.OpToken.Loc, "Return stack is empty")
		}
		sc.Addr = sc.ReturnStack.Pop().(types.IntType) + 1
		vm.Ctx.Memory.OperativeMemRegion.Ptr -= vm.Ctx.Scopes[sc.CurrentScopeName()].MemSize
		sc.FuncStack.Pop()
	case OpPushAlloc:
		alloc_name := op.Operand.(string)
		alloc, scope := vm.Ctx.GetAlloc(alloc_name, sc.CurrentScopeName())
		if scope == ScopeUnknown {
			lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Unknown alloc at runtime: `%s`", alloc_name))
		}
		addr := alloc.Offset
		if scope == ScopeLocal {
			addr += vm.Ctx.Memory.OperativeMemRegion.Ptr - vm.Ctx.Scopes[sc.CurrentScopeName()].MemSize
		} else {
			addr += vm.Ctx.Memory.OperativeMemRegion.Start
		}
		sc.Stack.Push(addr)
		sc.Addr++
	default:
		lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled operation: `%s`", OpName[op.Typ]))
	}
}

func (vm *VM) Prepare(ops []Op, args []string) *ScriptContext {
	len_ops := types.IntType(len(ops))
	sc := NewScriptContext(len_ops, args)
	sc.Addr = vm.Ctx.Funcs["main"]

	vm.Ctx.Memory.OperativeMemRegion.Ptr = vm.Ctx.Memory.OperativeMemRegion.Start + vm.Ctx.GlobalScope().MemSize
	return sc
}

func (vm *VM) Interprete(ops []Op, args []string) ExitCodeType {
	sc := vm.Prepare(ops, args)
	for sc.Addr < sc.OpsCount {
		vm.Step(ops, sc)
	}
	return sc.GetExitCode(ops)
}

func (vm *VM) InterpreteDebug(ops []Op, args []string, di *DebugInterface) {
	sc := vm.Prepare(ops, args)

loop:
	for {
		cmd := <-di.Commands

		switch cmd.Type {
		case DebugCmdStack: // print stack
			fmt.Println(sc.Stack.Data)
			di.SendOK()
		case DebugCmdMemory: // print memory
			vm.Ctx.Memory.PrintDebug()
			di.SendOK()
		case DebugCmdOperativeMemory:
			chunk := cmd.Args.([]int)
			start, size := chunk[0], chunk[1]
			if start >= int(vm.Ctx.Memory.MemorySize) {
				di.SendFailed(fmt.Sprintf("Start address %d is out of bounds", start))
				continue
			}
			end := start + size
			if end >= int(vm.Ctx.Memory.MemorySize) {
				end = int(vm.Ctx.Memory.MemorySize) - 1
			}
			memory_chunk := vm.Ctx.Memory.Data[start:end]
			fmt.Printf("addr=%d size=%d: %v\n", start, size, memory_chunk)
			di.SendOK()
		case DebugCmdPrint:
			names := cmd.Args.([]string)
			n_found := vm.Ctx.DebugConstNames(names, sc.CurrentScopeName()) +
				vm.Ctx.DebugAllocNames(names, sc.CurrentScopeName()) +
				vm.Ctx.DebugFuncNames(names)
			if n_found > 0 {
				di.SendOK()
			} else {
				di.SendFailed(fmt.Sprintf("Failed to print values for names: %v", names))
			}
		case DebugCmdQuit: // quit
			di.SendOK()
			break loop
		case DebugCmdStep: // step
			if sc.Addr >= sc.OpsCount {
				di.SendFailed("Can not step: script finished")
			} else {
				steps_count := cmd.Args.(int)
				for i := 0; i < steps_count && sc.Addr < sc.OpsCount; i++ {
					vm.Step(ops, sc)

					addr, is_bp := di.IsBreakpoint(sc, ops)
					if is_bp {
						fmt.Printf("[INFO] Break at address %d\n", addr)
						break
					}
				}
				if sc.Addr >= sc.OpsCount {
					ec := sc.GetExitCode(ops)
					fmt.Printf("[INFO] Script finished with exit code %d", ec.Code)
					if len(ec.Msg) > 0 {
						fmt.Printf(": %s", ec.Msg)
					}
					fmt.Println()
				}
				di.SendOK()
			}
		case DebugCmdContinue: // continue
			if sc.Addr >= sc.OpsCount {
				di.SendFailed("Can not continue: script finished")
			} else {
				for sc.Addr < sc.OpsCount {
					vm.Step(ops, sc)

					addr, is_bp := di.IsBreakpoint(sc, ops)
					if is_bp {
						fmt.Printf("[INFO] Break at address %d\n", addr)
						break
					}
				}
				if sc.Addr >= sc.OpsCount {
					ec := sc.GetExitCode(ops)
					fmt.Printf("[INFO] Script finished with exit code %d", ec.Code)
					if len(ec.Msg) > 0 {
						fmt.Printf(": %s", ec.Msg)
					}
					fmt.Println()
				}
				di.SendOK()
			}
		case DebugCmdBreakpointSet:
			bps := cmd.Args.(BreakPointList)

			found_names := make([]string, 0, len(bps.Funcs))
			for _, func_name := range bps.Funcs {
				addr, exists := vm.Ctx.Funcs[func_name]
				if !exists {
					fmt.Printf("[WARN] Can not set break point to unknown function `%s`, skip\n", func_name)
				} else {
					found_names = append(found_names, func_name)
					di.BreakPoints[addr] = true
				}
			}

			found_addr := make([]types.IntType, 0, len(bps.Addr))
			for _, addr := range bps.Addr {
				if addr >= sc.OpsCount || addr < 0 {
					fmt.Printf("[WARN] Address %d is out of bounds. skip\n", addr)
				} else {
					di.BreakPoints[addr] = true
					found_addr = append(found_addr, addr)
				}
			}

			if len(found_names) > 0 {
				fmt.Printf("[INFO] Set break point to functions %v\n", found_names)
			}
			if len(found_addr) > 0 {
				fmt.Printf("[INFO] Set break point for addresses %v\n", found_addr)
			}

			if len(found_names)+len(found_addr) > 0 {
				di.SendOK()
			} else {
				di.SendFailed(
					fmt.Sprintf("Can not set break points for functions=%v and addresses=%v", bps.Funcs, bps.Addr),
				)
			}
		case DebugCmdBreakpointList:
			addresses := make([]types.IntType, 0, len(di.BreakPoints))
			for addr := range di.BreakPoints {
				addresses = append(addresses, addr)
			}
			sort.Slice(addresses, func(i, j int) bool { return addresses[i] < addresses[j] })
			for _, addr := range addresses {
				fmt.Printf("b%s\n", ops[addr].Str(addr))
			}
			if len(addresses) == 0 {
				fmt.Println("[INFO] No breakpoints were set")
			}
			di.SendOK()
		case DebugCmdBreakpointRemove:
			bps := cmd.Args.(BreakPointList)

			removed_names := make([]string, 0, len(bps.Funcs))
			for _, func_name := range bps.Funcs {
				addr, exists := vm.Ctx.Funcs[func_name]
				if !exists {
					fmt.Printf("[WARN] Can not remove break point from unknown function `%s`\n", func_name)
				} else {
					_, exists := di.BreakPoints[addr]
					if !exists {
						fmt.Printf("[WARN] Can not remove break point from function `%s` - it was not set\n", func_name)
					} else {
						removed_names = append(removed_names, func_name)
						delete(di.BreakPoints, addr)
					}
				}
			}

			removed_addr := make([]types.IntType, 0, len(bps.Addr))
			for _, addr := range bps.Addr {
				_, exists := di.BreakPoints[addr]
				if !exists {
					fmt.Printf("[WARN] Can not remove break point from address `%d` - it was not set, skip\n", addr)
				} else {
					removed_addr = append(removed_addr, addr)
					delete(di.BreakPoints, addr)
				}
			}

			if len(removed_names) > 0 {
				fmt.Printf("[INFO] Remove break points from functions %v\n", removed_names)
			}
			if len(removed_addr) > 0 {
				fmt.Printf("[INFO] Remove break points from addresses %v\n", removed_addr)
			}

			if len(removed_names)+len(removed_addr) > 0 {
				di.SendOK()
			} else {
				di.SendFailed(
					fmt.Sprintf("Can not remove break points for functions=%v and addresses=%v", bps.Funcs, bps.Addr),
				)
			}
		case DebugCmdToken: // token
			if sc.Addr >= sc.OpsCount {
				di.SendFailed("Can not print token: script finished")
			} else {
				token := ops[sc.Addr].OpToken
				fmt.Printf("%s:%d:%d Token(%s)\n", token.Loc.Filepath, token.Loc.Line+1, token.Loc.Column+1, token.Text)
				di.SendOK()
			}
		case DebugCmdOperation: // operation
			if sc.Addr >= sc.OpsCount {
				di.SendFailed("Can not print operation: script finished")
			} else {
				context_size := cmd.Args.(types.IntType)
				di.PrintOpsList(sc.Addr-context_size, sc.Addr+context_size, ops, sc)
				di.SendOK()
			}
		case DebugCmdOperationList: // print ops list
			di.PrintOpsList(0, sc.OpsCount-1, ops, sc)
			di.SendOK()
		case DebugCmdEnv: // env - consts and allocs
			typ := cmd.Args.(string)
			if sc.Addr >= sc.OpsCount {
				di.SendFailed("Can not print environment: script finished")
			} else {
				current_scope_name := sc.CurrentScopeName()

				if typ == "all" || typ == "local" {
					fmt.Printf("Scope: <%s>\n", current_scope_name)
					if current_scope_name != GlobalScopeName {
						fmt.Printf("Locals: ")
						vm.Ctx.DebugConsts(current_scope_name)
						vm.Ctx.DebugAllocs(current_scope_name)
						fmt.Println()
					}
				}

				if typ == "all" || typ == "global" {
					fmt.Printf("Globals: ")
					vm.Ctx.DebugConsts(GlobalScopeName)
					vm.Ctx.DebugAllocs(GlobalScopeName)
					fmt.Println()
				}

				di.SendOK()
			}
		default:
			di.SendFailed(fmt.Sprintf("Unknown command: '%s'", cmd.Str))
		}
	}
}
