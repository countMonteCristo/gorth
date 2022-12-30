package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
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
func (vm *VM) parse_named_block(token *lexer.Token, tokens *[]lexer.Token, typ string) (tok lexer.Token, const_value int) {
	if len(*tokens) == 0 {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name, but got nothing", typ))
		utils.Exit(1)
	}

	tok, *tokens = (*tokens)[0], (*tokens)[1:]

	if tok.Typ != lexer.TokenWord {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name to be a word, but got `%s`", typ, tok.Text))
		utils.Exit(1)
	}
	defined_token, exists := vm.Ctx.Names[tok.Text]
	if exists {
		lexer.CompilerFatal(&tok.Loc, fmt.Sprintf("Redefinition of word `%s`", tok.Text))
		lexer.CompilerInfo(&defined_token.Loc, "Previously defined here")
		utils.Exit(1)
	}
	vm.Ctx.Names[tok.Text] = tok

	const_block := make([]lexer.Token, 0)
	for {
		if len(*tokens) == 0 {
			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unexpected end while processing `%s` block", typ))
			utils.Exit(1)
		}
		var btok lexer.Token
		btok, *tokens = (*tokens)[0], (*tokens)[1:]

		if btok.Typ == lexer.TokenKeyword && btok.Value.(lexer.KeywordType) == lexer.KeywordEnd {
			break
		}

		const_block = append(const_block, btok)
	}

	const_value = vm.const_eval(&tok, &const_block)
	return
}

func (vm *VM) const_eval(name_token *lexer.Token, tokens *[]lexer.Token) (value int) {
	const_stack := &utils.Stack{}
	for _, token := range *tokens {
		switch token.Typ {
		case lexer.TokenInt:
			const_stack.Push(token.Value.(int))
		case lexer.TokenWord:
			intrinsic, exists := lexer.WordToIntrinsic[token.Text]
			if exists {
				switch intrinsic {
				case lexer.IntrinsicPlus:
					b := const_stack.Pop().(int)
					a := const_stack.Pop().(int)
					const_stack.Push(a + b)
				case lexer.IntrinsicMinus:
					b := const_stack.Pop().(int)
					a := const_stack.Pop().(int)
					const_stack.Push(a - b)
				case lexer.IntrinsicMul:
					b := const_stack.Pop().(int)
					a := const_stack.Pop().(int)
					const_stack.Push(a * b)
				case lexer.IntrinsicDiv:
					b := const_stack.Pop().(int)
					if b == 0 {
						lexer.CompilerFatal(&token.Loc, "Division by zero")
						utils.Exit(1)
					}
					a := const_stack.Pop().(int)
					const_stack.Push(a / b)
				case lexer.IntrinsicMod:
					b := const_stack.Pop().(int)
					if b == 0 {
						lexer.CompilerFatal(&token.Loc, "Division by zero")
						utils.Exit(1)
					}
					a := const_stack.Pop().(int)
					const_stack.Push(a % b)
				case lexer.IntrinsicShl:
					b := const_stack.Pop().(int)
					if b < 0 {
						lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Negative shift amount in `<<`: %d", b))
						utils.Exit(1)
					}
					a := const_stack.Pop().(int)
					const_stack.Push(a << b)
				case lexer.IntrinsicShr:
					b := const_stack.Pop().(int)
					if b < 0 {
						lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Negative shift amount in `>>`: %d", b))
						utils.Exit(1)
					}
					a := const_stack.Pop().(int)
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
					utils.Exit(1)
				}
				continue
			}

			val, exists := vm.Ctx.Consts[token.Text]
			if exists {
				const_stack.Push(val)
				continue
			}

			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unsupported word in compile-time const-block evaluation: `%s`", token.Text))
			utils.Exit(1)
		default:
			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unsupported token in compile-time const-block evaluation: `%s`", token.Text))
			utils.Exit(1)
		}
	}

	if const_stack.Size() > 1 {
		lexer.CompilerFatal(&name_token.Loc, "Unhandled data in compile-time const-block evaluation stack")
		utils.Exit(1)
	}

	value = const_stack.Pop().(int)
	return
}

func (vm *VM) parse_func_def(token *lexer.Token, tokens *[]lexer.Token) (func_name string) {
	if len(*tokens) == 0 {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name, but got nothing", token.Text))
		utils.Exit(1)
	}

	var name_token lexer.Token
	name_token, *tokens = (*tokens)[0], (*tokens)[1:]

	if name_token.Typ != lexer.TokenWord {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `%s` name to be a word, but got `%s`", token.Text, name_token.Text))
		utils.Exit(1)
	}

	defined_token, exists := vm.Ctx.Names[name_token.Text]
	if exists {
		lexer.CompilerFatal(&name_token.Loc, fmt.Sprintf("Redefinition of word `%s` in function definition", name_token.Text))
		lexer.CompilerInfo(&defined_token.Loc, "Previously defined here")
		utils.Exit(1)
	}
	func_name = name_token.Text
	vm.Ctx.Names[func_name] = name_token

	var do_token lexer.Token
	do_token, *tokens = (*tokens)[0], (*tokens)[1:]

	if do_token.Typ != lexer.TokenKeyword {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected `do` to start the function name, but got `%s`", token.Text))
		utils.Exit(1)
	}
	if do_token.Value.(lexer.KeywordType) != lexer.KeywordDo {
		lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Expected keyword `do` to start the function name, but got `%s`", token.Text))
		utils.Exit(1)
	}

	return
}

func (vm *VM) preprocess_string_literals(tokens *[]lexer.Token) {
	address := 1
	for _, token := range *tokens {
		if token.Typ == lexer.TokenString {
			literal := token.Value.(string)
			_, exists := vm.Ctx.Memory.StringsMap[literal]
			if !exists {
				vm.Ctx.Memory.StringsMap[literal] = address
				address += len(literal) + 1 // save literals as null-terminated strings
			}
		}
	}

	vm.Ctx.Memory.StringsRegion = MemoryRegion{
		Start: 1,
		Size:  address - 1,
		Ptr:   address,
	}
}

func (vm *VM) Compile(tokens []lexer.Token, args []string) (ops []Op) {
	// assert(lexer.TokenCount == 6, "Unhandled Token in compile()")

	blocks := &utils.Stack{}

	current_function := ""

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
				Operand: token.Value.(int),
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
				Operand: len(literal),
				OpToken: token,
			})
		case lexer.TokenChar:
			ops = append(ops, Op{
				Typ:     OpPushInt,
				Operand: token.Value.(int),
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

			val, exists := vm.Ctx.Consts[name]
			if exists {
				ops = append(ops, Op{
					Typ:     OpPushInt,
					Operand: val,
					OpToken: token,
				})
				continue
			}

			ptr, exists := vm.Ctx.Allocs[name]
			if exists {
				ops = append(ops, Op{
					Typ:     OpPushInt,
					Operand: ptr,
					OpToken: token,
				})
				continue
			}

			func_addr, exists := vm.Ctx.Funcs[name]
			if exists {
				ops_count := len(ops)
				ops = append(ops, Op{
					Typ:     OpCall,
					Operand: func_addr - ops_count,
					OpToken: token,
				})
				continue
			}

			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unknown word: `%s`", token.Text))
			utils.Exit(1)

		case lexer.TokenKeyword:
			kw_type := token.Value.(lexer.KeywordType)
			op := Op{OpToken: token}
			switch kw_type {
			case lexer.KeywordIf:
				op.Typ = OpIf
				blocks.Push(Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case lexer.KeywordElse:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `end` found")
					utils.Exit(1)
				}
				block := blocks.Pop().(Block)
				if block.Tok.Typ != lexer.TokenKeyword {
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but not `%s`. Probably bug in lex()", block.Tok.Text))
					utils.Exit(1)
				}
				block_start_kw := block.Tok.Value.(lexer.KeywordType)
				switch block_start_kw {
				case lexer.KeywordIf:
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
				case lexer.KeywordElse:
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("`else` may only come after `if` block, but got `%s`. Probably bug in lex()", block.Tok.Text))
					utils.Exit(1)
				case lexer.KeywordEnd:
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("`else` may only come after `if` block, but got `%s`. Probably bug in lex()", block.Tok.Text))
					utils.Exit(1)
				case lexer.KeywordWhile:
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("`else` may only come after `if` block, but got `%s`. Probably bug in lex()", block.Tok.Text))
					utils.Exit(1)
				default:
					lexer.CompilerFatal(&token.Loc, "Unhandled block start processing in vm.Compile() at KeywordElse")
					utils.Exit(1)
				}

				op.Typ = OpElse
				blocks.Push(Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case lexer.KeywordEnd:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `end` found")
					utils.Exit(1)
				}
				block := blocks.Pop().(Block)
				if block.Tok.Typ != lexer.TokenKeyword {
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but not `%s`. Probably bug in lex()", block.Tok.Text))
					utils.Exit(1)
				}
				block_start_kw := block.Tok.Value.(lexer.KeywordType)
				op.Operand = 1
				switch block_start_kw {
				case lexer.KeywordIf:
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
					op.Typ = OpEnd
					ops = append(ops, op)
				case lexer.KeywordElse:
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
					op.Typ = OpEnd
					ops = append(ops, op)
				case lexer.KeywordWhile:
					lexer.CompilerFatal(&block.Tok.Loc, "`while` block must contain `do` before `end`")
					utils.Exit(1)
				case lexer.KeywordDo:
					for _, jump := range block.Jumps {
						switch jump.Keyword {
						case lexer.KeywordBreak:
							ops[jump.Addr].Operand = len(ops) - jump.Addr + 1
						case lexer.KeywordContinue:
							ops[jump.Addr].Operand = ops[block.Addr].Operand.(int) + (block.Addr - jump.Addr)
						default:
							lexer.CompilerFatal(&block.Tok.Loc, fmt.Sprintf("Unhandled jump-keyword in vm.Compile: `%s`", lexer.KeywordName[jump.Keyword]))
							utils.Exit(1)
						}
					}
					op.Operand = ops[block.Addr].Operand.(int) + (block.Addr - len(ops))
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
					op.Typ = OpEnd
					ops = append(ops, op)
				case lexer.KeywordEnd:
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("`end` may only close `if-else` or `while-do` blocks, but got `%s`. Probably bug in lex()", block.Tok.Text))
					utils.Exit(1)
				case lexer.KeywordBreak:
					lexer.CompilerFatal(&block.Tok.Loc, "`break` keyword shouldn't be in blocks stack")
					utils.Exit(1)
				case lexer.KeywordFunc:
					func_end_op := Op{
						OpToken: token, Typ: OpFuncEnd, Operand: current_function,
					}
					current_function = ""
					ops = append(ops, func_end_op)
				default:
					lexer.CompilerFatal(&token.Loc, "Unhandled block start processing in vm.Compile()")
					utils.Exit(1)
				}
			case lexer.KeywordWhile:
				op.Typ = OpWhile
				blocks.Push(Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case lexer.KeywordDo:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `do` found")
					utils.Exit(1)
				}
				block := blocks.Pop().(Block)
				if block.Tok.Typ != lexer.TokenKeyword {
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but not `%s`. Probably bug in lex()", block.Tok.Text))
					utils.Exit(1)
				}
				if block.Tok.Value.(lexer.KeywordType) != lexer.KeywordWhile {
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("`do` may come only inside `while` block, but not `%s`. Probably bug in lex()", block.Tok.Text))
					utils.Exit(1)
				}
				op.Typ = OpDo
				op.Operand = block.Addr - len(ops) // save relative address of `while`
				blocks.Push(Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case lexer.KeywordBreak:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `break` found")
					utils.Exit(1)
				}
				var i int

				for i = len(blocks.Data) - 1; i >= 0; i-- {
					cur_block := blocks.Data[i].(Block)

					if cur_block.Tok.Typ == lexer.TokenKeyword && cur_block.Tok.Value.(lexer.KeywordType) == lexer.KeywordDo {
						cur_block.Jumps = append(cur_block.Jumps, Jump{Keyword: lexer.KeywordBreak, Addr: len(ops)})
						blocks.Data[i] = cur_block
						break
					}
				}
				if i < 0 {
					lexer.CompilerFatal(&token.Loc, "Break should be inside while-loop, but it doesn't")
					utils.Exit(1)
				}

				op.Typ = OpBreak
				ops = append(ops, op)
			case lexer.KeywordContinue:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `break` found")
					utils.Exit(1)
				}
				var i int

				for i = len(blocks.Data) - 1; i >= 0; i-- {
					cur_block := blocks.Data[i].(Block)

					if cur_block.Tok.Typ == lexer.TokenKeyword && cur_block.Tok.Value.(lexer.KeywordType) == lexer.KeywordDo {
						cur_block.Jumps = append(cur_block.Jumps, Jump{Keyword: lexer.KeywordContinue, Addr: len(ops)})
						blocks.Data[i] = cur_block
						break
					}
				}
				if i < 0 {
					lexer.CompilerFatal(&token.Loc, "Break should be inside while-loop, but it doesn't")
					utils.Exit(1)
				}

				op.Typ = OpContinue
				ops = append(ops, op)

			case lexer.KeywordConst:
				if current_function != "" {
					lexer.CompilerFatal(&token.Loc, "Cannot define constants inside a function yet")
					utils.Exit(1)
				}
				tok, const_value := vm.parse_named_block(&token, &tokens, token.Text)
				vm.Ctx.Consts[tok.Text] = const_value
				vm.Ctx.Names[tok.Text] = tok
			case lexer.KeywordAlloc:
				if current_function != "" {
					lexer.CompilerFatal(&token.Loc, "Cannot allocate memory inside a function yet")
					utils.Exit(1)
				}
				tok, alloc_size := vm.parse_named_block(&token, &tokens, token.Text)
				if alloc_size < 0 {
					lexer.CompilerFatal(&tok.Loc, fmt.Sprintf("Negative size for `alloc` block: %d", alloc_size))
					utils.Exit(1)
				}

				vm.Ctx.Allocs[tok.Text] = vm.Ctx.Memory.OperativeMemRegion.Ptr
				vm.Ctx.Memory.OperativeMemRegion.Ptr += alloc_size
				vm.Ctx.Names[tok.Text] = tok

			case lexer.KeywordFunc:
				if current_function != "" {
					lexer.CompilerFatal(&token.Loc, "Cannot define functions inside a function")
					utils.Exit(1)
				}
				func_name := vm.parse_func_def(&token, &tokens)
				current_function = func_name

				vm.Ctx.Funcs[func_name] = len(ops)
				blocks.Push(Block{Addr: len(ops), Tok: token})

				op.Typ = OpFuncBegin
				op.Operand = func_name
				ops = append(ops, op)

			default:
				lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unhandled KewordType handling in vm.Compile(): `%s`", token.Text))
				utils.Exit(1)
			}
		default:
			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unhandled token: `%s`\n", token.Text))
			utils.Exit(1)
		}
	}

	if len(blocks.Data) > 0 {
		top := blocks.Data[len(blocks.Data)-1].(Block)
		lexer.CompilerFatal(&top.Tok.Loc, fmt.Sprintf("Unclosed `%s`-block", top.Tok.Text))
		utils.Exit(1)
	}

	_, exists := vm.Ctx.Funcs["main"]
	if !exists {
		fmt.Println("No entry point found (function `main` was not defined)")
		utils.Exit(1)
	}

	return
}

func (vm *VM) Interprete(ops []Op, args []string, debug bool) {
	stack := &utils.Stack{}

	return_stack := utils.Stack{}

	if debug {
		for addr, op := range ops {
			fmt.Println(op.Str(addr))
		}
		fmt.Println("---------------------------------")
	}

	// assert(OpCount == 9, "Unhandled Op in interprete()")
	addr := vm.Ctx.Funcs["main"]
	return_stack.Push(len(ops))

	for addr < len(ops) {

		op := ops[addr]
		// fmt.Printf("Process addr=%d stack=%v ret_stack=%v\n", addr, stack.Data, return_stack.Data)

		switch op.Typ {
		case OpPushInt:
			n := op.Operand.(int)
			stack.Push(n)
			addr++
		case OpPushBool:
			switch op.Operand {
			case types.BoolFalse:
				stack.Push(false)
			case types.BoolTrue:
				stack.Push(true)
			}
			addr++
		case OpIf:
			top := stack.Pop().(bool)
			if top {
				addr++
			} else {
				addr += op.Operand.(int)
			}
		case OpElse:
			addr += op.Operand.(int)
		case OpEnd:
			addr += op.Operand.(int)
		case OpWhile:
			addr++
		case OpDo:
			top := stack.Pop().(bool)
			if top {
				addr++
			} else {
				addr += op.Operand.(int)
			}
		case OpIntrinsic:
			// assert(lexer.IntrinsicCount == 36, "Unhandled intrinsic in interprete()")
			switch op.Operand {
			case lexer.IntrinsicPlus:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) + b.(int))
			case lexer.IntrinsicMinus:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) - b.(int))
			case lexer.IntrinsicMul:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) * b.(int))
			case lexer.IntrinsicDiv:
				b := stack.Pop()
				b_arg := b.(int)
				if b_arg == 0 {
					lexer.RuntimeFatal(&op.OpToken.Loc, "Division by zero")
				}
				a := stack.Pop()
				stack.Push(a.(int) / b_arg)
			case lexer.IntrinsicMod:
				b := stack.Pop()
				b_arg := b.(int)
				if b_arg == 0 {
					lexer.RuntimeFatal(&op.OpToken.Loc, "Division by zero")
				}
				a := stack.Pop()
				stack.Push(a.(int) % b_arg)
			case lexer.IntrinsicShl:
				b := stack.Pop()
				b_arg := b.(int)
				if b_arg < 0 {
					lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Negative shift amount in `<<`: %d", b_arg))
				}
				a := stack.Pop()
				stack.Push(a.(int) << b_arg)
			case lexer.IntrinsicShr:
				b := stack.Pop()
				b_arg := b.(int)
				if b_arg < 0 {
					lexer.RuntimeFatal(&op.OpToken.Loc, fmt.Sprintf("Negative shift amount in `>>`: %d", b_arg))
				}
				a := stack.Pop()
				stack.Push(a.(int) >> b_arg)
			case lexer.IntrinsicLogicalAnd:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(bool) && b.(bool))
			case lexer.IntrinsicLogicalOr:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(bool) || b.(bool))
			case lexer.IntrinsicLogicalNot:
				x := stack.Pop()
				stack.Push(!x.(bool))
			case lexer.IntrinsicBitAnd:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) & b.(int))
			case lexer.IntrinsicBitOr:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) | b.(int))
			case lexer.IntrinsicBitXor:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) ^ b.(int))
			case lexer.IntrinsicDup:
				x := stack.Top()
				stack.Push(x)
			case lexer.IntrinsicSwap:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(b)
				stack.Push(a)
			case lexer.IntrinsicDrop:
				_ = stack.Pop()
			case lexer.IntrinsicOver:
				x := stack.Data[len(stack.Data)-2]
				stack.Push(x)
			case lexer.IntrinsicRot:
				c := stack.Pop()
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(b)
				stack.Push(c)
				stack.Push(a)
			case lexer.IntrinsicEq:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) == b.(int))
			case lexer.IntrinsicNe:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) != b.(int))
			case lexer.IntrinsicLe:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) <= b.(int))
			case lexer.IntrinsicGe:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) >= b.(int))
			case lexer.IntrinsicLt:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) < b.(int))
			case lexer.IntrinsicGt:
				b := stack.Pop()
				a := stack.Pop()
				stack.Push(a.(int) > b.(int))
			case lexer.IntrinsicPuti:
				x := stack.Pop()
				fmt.Print(x)
			case lexer.IntrinsicPutc:
				x := stack.Pop()
				fmt.Print(string(byte(x.(int))))
			case lexer.IntrinsicPuts:
				size := stack.Pop().(int)
				ptr := stack.Pop().(int)
				str := string(vm.Ctx.Memory.Data[ptr : ptr+size])
				fmt.Print(str)
			case lexer.IntrinsicDebug:
				fmt.Printf("\tMem: %v\tStack: %v\n", vm.Ctx.Memory.Data[vm.Ctx.Memory.OperativeMemRegion.Start:vm.Ctx.Memory.OperativeMemRegion.Ptr], stack.Data)
			case lexer.IntrinsicLoad8:
				x := stack.Pop()
				ptr := x.(int)
				val := vm.Ctx.Memory.LoadFromMem(ptr, 1)
				stack.Push(val)
			case lexer.IntrinsicStore8:
				ptr := stack.Pop().(int)
				x := stack.Pop().(int)
				vm.Ctx.Memory.StoreToMem(ptr, x, 1)
			case lexer.IntrinsicLoad16:
				x := stack.Pop()
				ptr := x.(int)
				value := vm.Ctx.Memory.LoadFromMem(ptr, 2)
				stack.Push(value)
			case lexer.IntrinsicStore16:
				ptr := stack.Pop().(int)
				x := stack.Pop().(int)
				vm.Ctx.Memory.StoreToMem(ptr, x, 2)
			case lexer.IntrinsicLoad32:
				x := stack.Pop()
				ptr := x.(int)
				value := vm.Ctx.Memory.LoadFromMem(ptr, 4)
				stack.Push(value)
			case lexer.IntrinsicStore32:
				ptr := stack.Pop().(int)
				x := stack.Pop().(int)
				vm.Ctx.Memory.StoreToMem(ptr, x, 4)
			case lexer.IntrinsicLoad64:
				x := stack.Pop()
				ptr := x.(int)
				value := vm.Ctx.Memory.LoadFromMem(ptr, 8)
				stack.Push(value)
			case lexer.IntrinsicStore64:
				ptr := stack.Pop().(int)
				x := stack.Pop().(int)
				vm.Ctx.Memory.StoreToMem(ptr, x, 8)
			case lexer.IntrinsicArgc:
				stack.Push(len(args))
			case lexer.IntrinsicArgv:
				stack.Push(vm.Ctx.Memory.Argv)
			default:
				lexer.CompilerFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled intrinsic: `%s`", op.OpToken.Text))
				utils.Exit(1)
			}
			addr++
		case OpBreak:
			addr += op.Operand.(int)
		case OpContinue:
			addr += op.Operand.(int)
		case OpCall:
			if return_stack.Size() >= vm.RecursionLimit {
				lexer.RuntimeFatal(&op.OpToken.Loc, "Recursion limit exceeded")
			}
			return_stack.Push(addr)
			addr += op.Operand.(int)
		case OpFuncBegin:
			addr++
		case OpFuncEnd:
			if return_stack.Size() == 0 {
				lexer.CompilerFatal(&op.OpToken.Loc, "return stack is empty!")
				utils.Exit(1)
			}
			addr = return_stack.Pop().(int) + 1
		default:
			lexer.CompilerFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled operation in vm.Interprete: `%s`", OpName[op.Typ]))
			utils.Exit(1)
		}
	}

	if debug {
		fmt.Println("---------------------------------")
		vm.Ctx.Memory.PrintDebug()
		fmt.Println("---------------------------------")
	}
}
