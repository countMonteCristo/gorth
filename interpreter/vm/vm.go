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
func (vm *VM) parse_named_block(token *lexer.Token, tokens *[]lexer.Token, typ string, ctx *FuncContext) (name_token lexer.Token, const_value types.IntType) {
	if len(*tokens) == 0 {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name, but got nothing", typ))
	}

	name_token, *tokens = (*tokens)[0], (*tokens)[1:]

	if name_token.Typ != lexer.TokenWord {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name to be a word, but got `%s`", typ, name_token.Text))
	}
	defined_token, exists := ctx.Names[name_token.Text]
	if exists {
		lexer.CompilerInfo(&name_token.Loc, fmt.Sprintf("Redefinition of word `%s`", name_token.Text))
		lexer.CompilerInfo(&defined_token.Loc, "Previously defined here")
		utils.Exit(1)
	}
	defined_token, exists = vm.Ctx.LocalContexts[""].Names[name_token.Text]
	_, func_exists := vm.Ctx.Funcs[name_token.Text]
	if exists && func_exists {
		lexer.CompilerInfo(&name_token.Loc, fmt.Sprintf("Redefinition of word `%s`", name_token.Text))
		lexer.CompilerInfo(&defined_token.Loc, "Previously defined here")
		utils.Exit(1)
	}

	ctx.Names[name_token.Text] = name_token

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

	const_value = vm.const_eval(&name_token, &const_block, ctx)
	return
}

func (vm *VM) const_eval(name_token *lexer.Token, tokens *[]lexer.Token, ctx *FuncContext) (value types.IntType) {
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

				default:
					lexer.CompilerFatal(
						&token.Loc,
						fmt.Sprintf(
							"Unexpected intrinsic in const-block compile-time "+
								"evaluation: `%s`. Supported: [+, -, *, /, %%, >>, <<]",
							token.Text,
						),
					)
				}
				continue
			}

			val, exists := vm.Ctx.GetConst(token.Text, ctx.FuncName)
			if exists {
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

func (vm *VM) parse_func_def(token *lexer.Token, tokens *[]lexer.Token, global_ctx *FuncContext) (name_token lexer.Token, func_name string) {
	if len(*tokens) == 0 {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name, but got nothing", token.Text))
	}

	name_token, *tokens = (*tokens)[0], (*tokens)[1:]

	if name_token.Typ != lexer.TokenWord {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name to be a word, but got `%s`", token.Text, name_token.Text))
	}

	defined_token, exists := global_ctx.Names[name_token.Text]
	if exists {
		lexer.CompilerInfo(&name_token.Loc, fmt.Sprintf("Redefinition of word `%s` in function definition", name_token.Text))
		lexer.CompilerInfo(&defined_token.Loc, "Previously defined here")
		utils.Exit(1)
	}

	func_name = name_token.Text
	global_ctx.Names[func_name] = name_token

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

	current_function := ""

	vm.preprocess_string_literals(&tokens)
	vm.Ctx.Memory.Prepare(args)

	globals := vm.Ctx.LocalContexts[""]

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

			val, exists := vm.Ctx.GetConst(name, current_function)
			if exists {
				ops = append(ops, Op{
					Typ:     OpPushInt,
					Operand: val,
					OpToken: token,
				})
				continue
			}

			_, scope := vm.Ctx.GetAlloc(name, current_function)
			if scope != ScopeUnknown {
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

			locals := vm.Ctx.LocalContexts[current_function]

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
						OpToken: token, Typ: OpFuncEnd, Operand: current_function,
					}
					current_function = "" // we do not allow nested function definitions so it is ok
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
				tok, const_value := vm.parse_named_block(&token, &tokens, token.Text, locals)
				locals.Consts[tok.Text] = const_value
			case lexer.KeywordAlloc:
				tok, alloc_size := vm.parse_named_block(&token, &tokens, token.Text, locals)
				if alloc_size < 0 {
					lexer.CompilerFatal(&tok.Loc, fmt.Sprintf("Negative size for `alloc` block: %d", alloc_size))
				}
				locals.Allocs[tok.Text] = Allocation{
					Offset: locals.MemSize, Size: alloc_size,
				}
				locals.MemSize += alloc_size

			case lexer.KeywordFunc:
				if current_function != "" {
					lexer.CompilerFatal(
						&token.Loc,
						fmt.Sprintf("Cannot define functions inside a function %s", current_function),
					)
				}
				func_token, func_name := vm.parse_func_def(&token, &tokens, globals)
				current_function = func_name

				new_ctx := NewFuncContext(func_name)
				new_ctx.Names[func_name] = func_token
				vm.Ctx.LocalContexts[func_name] = new_ctx

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

type ScriptContext struct {
	Stack       utils.Stack
	ReturnStack utils.Stack
	FuncStack   utils.Stack
	Addr        types.IntType
	Args        []string
	OpsCount    types.IntType
}

func NewScriptContext(len_ops types.IntType, args []string) *ScriptContext {
	ctx := &ScriptContext{
		Stack: utils.Stack{}, ReturnStack: utils.Stack{},
		Addr: 0, Args: args, OpsCount: len_ops, FuncStack: utils.Stack{},
	}
	ctx.ReturnStack.Push(len_ops)
	ctx.FuncStack.Push("")
	return ctx
}

func (ctx *ScriptContext) CurrentFunction() string {
	return ctx.FuncStack.Top().(string)
}

func (vm *VM) ProcessSyscall(ctx *ScriptContext) {
	syscall_id := ctx.Stack.Pop().(types.IntType)
	switch syscall_id {
	case unix.SYS_OPEN:
		mode := ctx.Stack.Pop().(types.IntType)
		flags := ctx.Stack.Pop().(types.IntType)
		ptr := ctx.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_OPEN, uintptr(unsafe.Pointer(&vm.Ctx.Memory.Data[ptr])), uintptr(flags), uintptr(mode),
		)
		ctx.Stack.Push(types.IntType(r1))
		ctx.Stack.Push(types.IntType(err))
	case unix.SYS_READ:
		count := ctx.Stack.Pop().(types.IntType)
		ptr := ctx.Stack.Pop().(types.IntType)
		fd := ctx.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_READ, uintptr(fd), uintptr(unsafe.Pointer(&vm.Ctx.Memory.Data[ptr])), uintptr(count),
		)
		ctx.Stack.Push(types.IntType(r1))
		ctx.Stack.Push(types.IntType(err))
	case unix.SYS_CLOSE:
		fd := ctx.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_CLOSE, uintptr(fd), 0, 0,
		)
		ctx.Stack.Push(types.IntType(r1))
		ctx.Stack.Push(types.IntType(err))
	case unix.SYS_WRITE:
		count := ctx.Stack.Pop().(types.IntType)
		ptr := ctx.Stack.Pop().(types.IntType)
		fd := ctx.Stack.Pop().(types.IntType)
		r1, _, err := unix.Syscall(
			unix.SYS_WRITE, uintptr(fd), uintptr(unsafe.Pointer(&vm.Ctx.Memory.Data[ptr])), uintptr(count),
		)
		ctx.Stack.Push(types.IntType(r1))
		ctx.Stack.Push(types.IntType(err))
	default:
		panic(fmt.Sprintf("Syscall #%d is not implemented yet\n", syscall_id))
	}
}

func (vm *VM) Step(ops []Op, ctx *ScriptContext) {
	op := ops[ctx.Addr]
	switch op.Typ {
	case OpPushInt, OpPushBool:
		ctx.Stack.Push(op.Operand)
		ctx.Addr++
	case OpIf, OpDo:
		top := ctx.Stack.Pop().(types.BoolType)
		if top {
			ctx.Addr++
		} else {
			ctx.Addr += op.Operand.(types.IntType)
		}
	case OpElse, OpEnd, OpBreak, OpContinue:
		ctx.Addr += op.Operand.(types.IntType)
	case OpWhile:
		ctx.Addr++
	case OpIntrinsic:
		intrinsic := op.Operand.(lexer.IntrinsicType)
		switch intrinsic {
		case lexer.IntrinsicPlus, lexer.IntrinsicMinus, lexer.IntrinsicMul, lexer.IntrinsicBitAnd, lexer.IntrinsicBitOr, lexer.IntrinsicBitXor:
			b := ctx.Stack.Pop().(types.IntType)
			a := ctx.Stack.Pop().(types.IntType)
			ctx.Stack.Push(SafeArithmeticFunctions[intrinsic](a, b))
		case lexer.IntrinsicDiv:
			b := ctx.Stack.Pop().(types.IntType)
			if b == 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, "Division by zero")
			}
			a := ctx.Stack.Pop().(types.IntType)
			ctx.Stack.Push(a / b)
		case lexer.IntrinsicMod:
			b := ctx.Stack.Pop().(types.IntType)
			if b == 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, "Division by zero")
			}
			a := ctx.Stack.Pop().(types.IntType)
			ctx.Stack.Push(a % b)
		case lexer.IntrinsicShl:
			b := ctx.Stack.Pop().(types.IntType)
			if b < 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Negative shift amount in `<<`: %d", b))
			}
			a := ctx.Stack.Pop().(types.IntType)
			ctx.Stack.Push(a << b)
		case lexer.IntrinsicShr:
			b := ctx.Stack.Pop().(types.IntType)
			if b < 0 {
				lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Negative shift amount in `>>`: %d", b))
			}
			a := ctx.Stack.Pop().(types.IntType)
			ctx.Stack.Push(a >> b)
		case lexer.IntrinsicBitNot:
			a := ctx.Stack.Pop().(types.IntType)
			ctx.Stack.Push(^a)
		case lexer.IntrinsicLogicalAnd, lexer.IntrinsicLogicalOr:
			b := ctx.Stack.Pop().(types.BoolType)
			a := ctx.Stack.Pop().(types.BoolType)
			ctx.Stack.Push(LogicalFunctions[intrinsic](a, b))
		case lexer.IntrinsicLogicalNot:
			x := ctx.Stack.Pop()
			ctx.Stack.Push(!x.(types.BoolType))
		case lexer.IntrinsicDup:
			x := ctx.Stack.Top()
			ctx.Stack.Push(x)
		case lexer.IntrinsicSwap:
			b := ctx.Stack.Pop()
			a := ctx.Stack.Pop()
			ctx.Stack.Push(b)
			ctx.Stack.Push(a)
		case lexer.IntrinsicDrop:
			ctx.Stack.Pop()
		case lexer.IntrinsicOver:
			x := ctx.Stack.Data[len(ctx.Stack.Data)-2]
			ctx.Stack.Push(x)
		case lexer.IntrinsicRot:
			c := ctx.Stack.Pop()
			b := ctx.Stack.Pop()
			a := ctx.Stack.Pop()
			ctx.Stack.Push(b)
			ctx.Stack.Push(c)
			ctx.Stack.Push(a)
		case lexer.IntrinsicEq, lexer.IntrinsicNe, lexer.IntrinsicLe, lexer.IntrinsicGe, lexer.IntrinsicLt, lexer.IntrinsicGt:
			b := ctx.Stack.Pop().(types.IntType)
			a := ctx.Stack.Pop().(types.IntType)
			ctx.Stack.Push(ComparableFunctions[intrinsic](a, b))
		case lexer.IntrinsicPuti:
			x := ctx.Stack.Pop()
			fmt.Print(x)
		case lexer.IntrinsicPutc:
			x := ctx.Stack.Pop()
			fmt.Print(string(byte(x.(types.IntType))))
		case lexer.IntrinsicPuts:
			size := ctx.Stack.Pop().(types.IntType)
			ptr := ctx.Stack.Pop().(types.IntType)
			str := string(vm.Ctx.Memory.Data[ptr : ptr+size])
			fmt.Print(str)
		case lexer.IntrinsicDebug:
			fmt.Printf(
				"\tMem: %v\tStack: %v\n",
				vm.Ctx.Memory.Data[vm.Ctx.Memory.OperativeMemRegion.Start:vm.Ctx.Memory.OperativeMemRegion.Ptr],
				ctx.Stack.Data,
			)
		case lexer.IntrinsicLoad8, lexer.IntrinsicLoad16, lexer.IntrinsicLoad32, lexer.IntrinsicLoad64:
			ptr := ctx.Stack.Pop().(types.IntType)
			val := vm.Ctx.Memory.LoadFromMem(ptr, LoadSizes[intrinsic])
			ctx.Stack.Push(val)
		case lexer.IntrinsicStore8, lexer.IntrinsicStore16, lexer.IntrinsicStore32, lexer.IntrinsicStore64:
			ptr := ctx.Stack.Pop().(types.IntType)
			x := ctx.Stack.Pop().(types.IntType)
			vm.Ctx.Memory.StoreToMem(ptr, x, StoreSizes[intrinsic])

		case lexer.IntrinsicArgc:
			ctx.Stack.Push(types.IntType(len(ctx.Args)))
		case lexer.IntrinsicArgv:
			ctx.Stack.Push(vm.Ctx.Memory.Argv)
		case lexer.IntrinsicSyscall:
			vm.ProcessSyscall(ctx)
		default:
			lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled intrinsic: `%s`", op.OpToken.Text))
		}
		ctx.Addr++
	case OpCall:
		if ctx.ReturnStack.Size() >= vm.RecursionLimit {
			lexer.RuntimeFatal(&op.OpToken.Loc, "Recursion limit exceeded")
		}
		ctx.ReturnStack.Push(ctx.Addr)
		ctx.Addr += op.Operand.(types.IntType)
	case OpFuncBegin:
		ctx.Addr++
		ctx.FuncStack.Push(op.Operand.(string))
		vm.Ctx.Memory.OperativeMemRegion.Ptr += vm.Ctx.LocalContexts[ctx.CurrentFunction()].MemSize
	case OpFuncEnd:
		if ctx.ReturnStack.Size() == 0 {
			lexer.RuntimeFatal(&op.OpToken.Loc, "Return stack is empty")
		}
		ctx.Addr = ctx.ReturnStack.Pop().(types.IntType) + 1
		vm.Ctx.Memory.OperativeMemRegion.Ptr -= vm.Ctx.LocalContexts[ctx.CurrentFunction()].MemSize
		ctx.FuncStack.Pop()
	case OpPushAlloc:
		alloc_name := op.Operand.(string)
		alloc, scope := vm.Ctx.GetAlloc(alloc_name, ctx.CurrentFunction())
		if scope == ScopeUnknown {
			lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Unknown alloc at runtime: `%s`", alloc_name))
		}
		addr := alloc.Offset
		if scope == ScopeLocal {
			addr += vm.Ctx.Memory.OperativeMemRegion.Ptr - vm.Ctx.LocalContexts[ctx.CurrentFunction()].MemSize
		} else {
			addr += vm.Ctx.Memory.OperativeMemRegion.Start
		}
		ctx.Stack.Push(addr)
		ctx.Addr++
	default:
		lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled operation: `%s`", OpName[op.Typ]))
	}
}

func (vm *VM) Prepare(ops []Op, args []string) *ScriptContext {
	len_ops := types.IntType(len(ops))
	ctx := NewScriptContext(len_ops, args)
	ctx.Addr = vm.Ctx.Funcs["main"]

	vm.Ctx.Memory.OperativeMemRegion.Ptr = vm.Ctx.Memory.OperativeMemRegion.Start + vm.Ctx.LocalContexts[""].MemSize
	return ctx
}

func (vm *VM) Interprete(ops []Op, args []string) {
	ctx := vm.Prepare(ops, args)
	for ctx.Addr < ctx.OpsCount {
		vm.Step(ops, ctx)
	}
}

func (vm *VM) InterpreteDebug(ops []Op, args []string, di *DebugInterface) {
	ctx := vm.Prepare(ops, args)

loop:
	for {
		cmd := <-di.Commands

		switch cmd.Type {
		case DebugCmdStack: // print stack
			fmt.Println(ctx.Stack.Data)
			di.SendOK()
		case DebugCmdMemory: // print memory
			vm.Ctx.Memory.PrintDebug()
			di.SendOK()
		case DebugCmdQuit: // quit
			di.SendOK()
			break loop
		case DebugCmdStep: // step
			if ctx.Addr >= ctx.OpsCount {
				di.SendFailed("Can not step: script finished")
			} else {
				steps_count := cmd.Args.(int)
				for i := 0; i < steps_count && ctx.Addr < ctx.OpsCount; i++ {
					vm.Step(ops, ctx)

					addr, is_bp := di.IsBreakpoint(ctx, ops)
					if is_bp {
						fmt.Printf("[INFO] Break at address %d\n", addr)
						break
					}
				}
				if ctx.Addr >= ctx.OpsCount {
					fmt.Printf("[INFO] Script finished\n")
				}
				di.SendOK()
			}
		case DebugCmdContinue: // continue
			if ctx.Addr >= ctx.OpsCount {
				di.SendFailed("Can not continue: script finished")
			} else {
				for ctx.Addr < ctx.OpsCount {
					vm.Step(ops, ctx)

					addr, is_bp := di.IsBreakpoint(ctx, ops)
					if is_bp {
						fmt.Printf("[INFO] Break at address %d\n", addr)
						break
					}
				}
				if ctx.Addr >= ctx.OpsCount {
					fmt.Printf("[INFO] Script finished\n")
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
				if addr >= ctx.OpsCount || addr < 0 {
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
			if ctx.Addr >= ctx.OpsCount {
				di.SendFailed("Can not print token: script finished")
			} else {
				token := ops[ctx.Addr].OpToken
				fmt.Printf("%s:%d:%d Token(%s)\n", token.Loc.Filepath, token.Loc.Line+1, token.Loc.Column+1, token.Text)
				di.SendOK()
			}
		case DebugCmdOperation: // operation
			if ctx.Addr >= ctx.OpsCount {
				di.SendFailed("Can not print operation: script finished")
			} else {
				context_size := cmd.Args.(types.IntType)
				start := ctx.Addr - context_size
				finish := ctx.Addr + context_size

				for addr := start; addr >= 0 && addr < ctx.OpsCount && addr <= finish; addr++ {
					marker := " "
					if addr == ctx.Addr {
						marker = "*"
					}
					op := ops[addr]
					fmt.Printf(
						"%s:%d:%d %s%s\n", op.OpToken.Loc.Filepath, op.OpToken.Loc.Line+1,
						op.OpToken.Loc.Column+1, marker, op.Str(addr),
					)
				}

				di.SendOK()
			}
		case DebugCmdOperationList: // print ops list
			for addr, op := range ops {
				iaddr := int64(addr)

				marker := " "
				if iaddr == ctx.Addr {
					marker = "*"
				}
				bp := " "
				_, exists := di.BreakPoints[iaddr]
				if exists {
					bp = "b"
				}

				fmt.Printf("%s%s%s\n", marker, bp, op.Str(int64(addr)))
			}
			di.SendOK()
		case DebugCmdEnv: // env - consts and allocs
			if ctx.Addr >= ctx.OpsCount {
				di.SendFailed("Can not print environment: script finished")
			} else {
				locals := vm.Ctx.LocalContexts[ctx.CurrentFunction()]

				fmt.Printf("Scope: <%s>\n", ctx.CurrentFunction())
				if ctx.CurrentFunction() != "" {
					fmt.Printf("Locals: ")
					for const_name, const_value := range locals.Consts {
						fmt.Printf("%s=%d ", const_name, const_value)
					}
					for alloc_name, alloc := range locals.Allocs {
						alloc_ptr := vm.Ctx.Memory.OperativeMemRegion.Ptr - vm.Ctx.LocalContexts[ctx.CurrentFunction()].MemSize + alloc.Offset
						alloc_mem := vm.Ctx.Memory.Data[alloc_ptr : alloc_ptr+alloc.Size]
						fmt.Printf("%s(%d,%d)=%d ", alloc_name, alloc_ptr, alloc.Size, alloc_mem)
					}
					fmt.Println()
				}

				fmt.Printf("Globals: ")
				for const_name, const_value := range vm.Ctx.LocalContexts[""].Consts {
					fmt.Printf("%s=%d ", const_name, const_value)
				}
				for alloc_name, alloc := range vm.Ctx.LocalContexts[""].Allocs {
					alloc_ptr := vm.Ctx.Memory.OperativeMemRegion.Start + alloc.Offset
					alloc_mem := vm.Ctx.Memory.Data[alloc_ptr : alloc_ptr+alloc.Size]
					fmt.Printf("%s(%d,%d)=%d ", alloc_name, alloc_ptr, alloc.Size, alloc_mem)
				}
				fmt.Println()

				di.SendOK()
			}
		default:
			di.SendFailed(fmt.Sprintf("Unknown command: '%s'", cmd.Str))
		}
	}
}
