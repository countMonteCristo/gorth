package vm

import (
	"GoStudy/Gorth/interpreter/lexer"
	"GoStudy/Gorth/interpreter/types"
	"GoStudy/Gorth/interpreter/utils"
	"fmt"
)

type VM struct {
	Ctx lexer.Context
}

func InitVM() *VM {
	vm := VM{
		Ctx: *lexer.InitContext(640*1024, 2*1024),
	}

	return &vm
}

func (vm *VM) Compile(tokens []lexer.Token) (ops []Op) {
	// assert(lexer.TokenCount == 6, "Unhandled Token in compile()")

	blocks := &utils.Stack{}

	for _, token := range tokens {

		switch token.Typ {
		case lexer.TokenInt:
			ops = append(ops, Op{
				Typ:     OpPushInt,
				Operand: token.Value.(int),
				OpToken: token,
			})
		case lexer.TokenString:
			vm.Ctx.StringLiterals = append(vm.Ctx.StringLiterals, token.Value.(string))
			ops = append(ops, Op{
				Typ:     OpPushStr,
				Operand: len(vm.Ctx.StringLiterals) - 1,
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

			_, exists := lexer.WordToIntrinsic[name]
			if exists {
				ops = append(ops, Op{
					Typ:     OpIntrinsic,
					Operand: token.Value.(lexer.IntrinsicType),
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

			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unknown word %s: probably bug in next_token", token.Text))
			utils.Exit(1)

		case lexer.TokenKeyword:
			kw_type := token.Value.(lexer.KeywordType)
			op := Op{OpToken: token}
			switch kw_type {
			case lexer.KeywordIf:
				op.Typ = OpIf
				blocks.Push(lexer.Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case lexer.KeywordElse:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `end` found")
					utils.Exit(1)
				}
				block := blocks.Pop().(lexer.Block)
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
				blocks.Push(lexer.Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case lexer.KeywordEnd:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `end` found")
					utils.Exit(1)
				}
				block := blocks.Pop().(lexer.Block)
				if block.Tok.Typ != lexer.TokenKeyword {
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Only keywords may form blocks, but not `%s`. Probably bug in lex()", block.Tok.Text))
					utils.Exit(1)
				}
				block_start_kw := block.Tok.Value.(lexer.KeywordType)
				op.Operand = 1
				switch block_start_kw {
				case lexer.KeywordIf:
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
				case lexer.KeywordElse:
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
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
							lexer.CompilerFatal(&block.Tok.Loc, fmt.Sprintf("Unhandled jump-keyword in vm.Compile: %s", lexer.KeywordName[jump.Keyword]))
							utils.Exit(1)
						}
					}
					op.Operand = ops[block.Addr].Operand.(int) + (block.Addr - len(ops))
					ops[block.Addr].Operand = len(ops) - block.Addr + 1
				case lexer.KeywordEnd:
					lexer.CompilerFatal(&token.Loc, fmt.Sprintf("`end` may only close `if-else` or `while-do` blocks, but got `%s`. Probably bug in lex()", block.Tok.Text))
					utils.Exit(1)
				case lexer.KeywordBreak:
					lexer.CompilerFatal(&block.Tok.Loc, "`break` keyword shouldn't be in blocks stack")
					utils.Exit(1)
				default:
					lexer.CompilerFatal(&token.Loc, "Unhandled block start processing in vm.Compile()")
					utils.Exit(1)
				}
				op.Typ = OpEnd
				ops = append(ops, op)
			case lexer.KeywordWhile:
				op.Typ = OpWhile
				blocks.Push(lexer.Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case lexer.KeywordDo:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `do` found")
					utils.Exit(1)
				}
				block := blocks.Pop().(lexer.Block)
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
				blocks.Push(lexer.Block{Addr: len(ops), Tok: token})
				ops = append(ops, op)
			case lexer.KeywordBreak:
				if blocks.Size() == 0 {
					lexer.CompilerFatal(&token.Loc, "Unexpected `break` found")
					utils.Exit(1)
				}
				var i int

				for i = len(blocks.Data) - 1; i >= 0; i-- {
					cur_block := blocks.Data[i].(lexer.Block)

					if cur_block.Tok.Typ == lexer.TokenKeyword && cur_block.Tok.Value.(lexer.KeywordType) == lexer.KeywordDo {
						cur_block.Jumps = append(cur_block.Jumps, lexer.Jump{Keyword: lexer.KeywordBreak, Addr: len(ops)})
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
					cur_block := blocks.Data[i].(lexer.Block)

					if cur_block.Tok.Typ == lexer.TokenKeyword && cur_block.Tok.Value.(lexer.KeywordType) == lexer.KeywordDo {
						cur_block.Jumps = append(cur_block.Jumps, lexer.Jump{Keyword: lexer.KeywordContinue, Addr: len(ops)})
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
			default:
				lexer.CompilerFatal(&token.Loc, fmt.Sprintf("Unhandled KewordType handling in vm.Compile(): %s", token.Text))
				utils.Exit(1)
			}
		default:
			lexer.CompilerFatal(&token.Loc, fmt.Sprintf("ERROR: Unhandled token: %s\n", token.Text))
			utils.Exit(1)
		}
	}

	if len(blocks.Data) > 0 {
		top := blocks.Data[len(blocks.Data)-1].(lexer.Block)
		lexer.CompilerFatal(&top.Tok.Loc, fmt.Sprintf("Unclosed %s-block", top.Tok.Text))
		utils.Exit(1)
	}
	return
}

func (vm *VM) Interprete(ops []Op, debug bool) {
	stack := &utils.Stack{}

	if debug {
		for addr, op := range ops {
			fmt.Println(op.Str(addr))
		}
		fmt.Println("---------------------------------")
	}

	// assert(OpCount == 9, "Unhandled Op in interprete()")
	addr := 0
	for addr < len(ops) {

		op := ops[addr]
		// fmt.Printf("Process addr=%d stack=%v\n", addr, stack.Data)

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
		case OpPushStr:
			ptr := op.Operand.(int)
			stack.Push(ptr)
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
				x := stack.Pop()
				index := x.(int)
				str := vm.Ctx.StringLiterals[index]
				fmt.Print(str)
			case lexer.IntrinsicDebug:
				fmt.Printf("\tMem: %v\tStack: %v\n", vm.Ctx.Memory.Data[:vm.Ctx.Memory.MemPtr], stack.Data)
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
			default:
				lexer.CompilerFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled intrinsic: %s", op.OpToken.Text))
			}
			addr++
		case OpBreak:
			addr += op.Operand.(int)
		case OpContinue:
			addr += op.Operand.(int)
		default:
			lexer.CompilerFatal(&op.OpToken.Loc, fmt.Sprintf("Unhandled operation in vm.Interprete: %s", OpName[op.Typ]))
			utils.Exit(1)
		}
	}

	if debug {
		fmt.Println("---------------------------------")
		fmt.Printf("Allocated: %d byte(s) total\n", vm.Ctx.Memory.MemPtr)
		fmt.Printf("Memory: %v\n", vm.Ctx.Memory.Data[:vm.Ctx.Memory.MemPtr])
		fmt.Println("---------------------------------")
	}
}
