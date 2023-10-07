package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
)

type Compiler struct {
	Blocks utils.Stack
	Ops    []Op
	debug  bool
}

func NewCompiler(debug bool) *Compiler {
	return &Compiler{Blocks: utils.Stack{}, Ops: make([]Op, 0), debug: debug}
}

func (c *Compiler) getCurrentAddr() (addr types.IntType) {
	return types.IntType(len(c.Ops))
}

func (c *Compiler) pushOps(scope_name string, ops ...Op) {
	if c.debug {
		for i := range ops {
			ops[i].DebugInfo = scope_name
		}
	}
	c.Ops = append(c.Ops, ops...)
}

func (c *Compiler) compileTokenInt(token *lexer.Token, scope_name string) error {
	c.pushOps(scope_name, Op{Typ: OpPushInt, Operand: token.Value.(types.IntType), OpToken: *token})
	return nil
}

func (c *Compiler) compileTokenString(token *lexer.Token, ctx *CompileTimeContext, scope_name string) error {
	literal := token.Value.(string)
	literal_addr, exists := ctx.StringsMap[literal]
	if !exists {
		return logger.FormatErrMsg(&token.Loc, "Unknown string literal at compile-time: `%s`", literal)
	}
	c.pushOps(scope_name,
		Op{Typ: OpPushInt, Operand: literal_addr, OpToken: *token},
		Op{Typ: OpPushInt, Operand: types.IntType(len(literal)), OpToken: *token},
	)
	return nil
}

func (c *Compiler) compileTokenChar(token *lexer.Token, scope_name string) error {
	c.pushOps(scope_name, Op{Typ: OpPushInt, Operand: token.Value.(types.IntType), OpToken: *token})
	return nil
}

func (c *Compiler) compileTokenBool(token *lexer.Token, scope_name string) error {
	c.pushOps(scope_name, Op{Typ: OpPushBool, Operand: token.Value.(types.BoolType), OpToken: *token})
	return nil
}

func (c *Compiler) compileTokenIntrinsic(token *lexer.Token, scope_name string, intrinsic lexer.IntrinsicType) error {
	if intrinsic == lexer.IntrinsicOffset || intrinsic == lexer.IntrinsicReset {
		return logger.FormatErrMsg(&token.Loc, "`%s` intrinsic is not allowed outside `const` or `alloc` blocks", token.Text)
	}
	c.pushOps(scope_name, Op{Typ: OpIntrinsic, Operand: intrinsic, OpToken: *token})
	return nil
}

func (c *Compiler) compileLocalConst(token *lexer.Token, val types.IntType, scope_name string) error {
	c.pushOps(scope_name, Op{Typ: OpPushInt, Operand: val, OpToken: *token})
	return nil
}

func (c *Compiler) compileGlobalConst(token *lexer.Token, val types.IntType, scope_name string) error {
	c.pushOps(scope_name, Op{Typ: OpPushInt, Operand: val, OpToken: *token})
	return nil
}

func (c *Compiler) compileLocalAlloc(token *lexer.Token, scope *Scope) error {
	c.pushOps(scope.ScopeName, Op{Typ: OpPushLocalAlloc, Operand: scope.MemSize - scope.Allocs[token.Text].Offset, OpToken: *token})
	return nil
}

func (c *Compiler) compileGlobalAlloc(token *lexer.Token, scope *Scope) error {
	c.pushOps(scope.ScopeName, Op{Typ: OpPushGlobalAlloc, Operand: scope.Allocs[token.Text].Offset, OpToken: *token})
	return nil
}

func (c *Compiler) compileFuncCall(token *lexer.Token, f *Function, scope_name string) error {
	c.pushOps(scope_name, Op{Typ: OpCall, Operand: f.Addr - c.getCurrentAddr(), OpToken: *token})
	return nil
}

func (c *Compiler) compileIfBlock(token *lexer.Token, th *lexer.TokenHolder, ctx *CompileTimeContext, scope_name string) error {
	c.Blocks.Push(NewBlock(c.getCurrentAddr(), token, lexer.KeywordIf))
	c.pushOps(scope_name, Op{OpToken: *token, Typ: OpIf})
	return c.compile(th, ctx, scope_name)
}

func (c *Compiler) compileElseBlock(token *lexer.Token, th *lexer.TokenHolder, ctx *CompileTimeContext, scope_name string) error {

	if c.Blocks.Size() == 0 {
		return logger.FormatErrMsg(&token.Loc, "Unexpected `end` found")
	}
	block := c.Blocks.Pop().(*Block)

	if block.Typ != lexer.KeywordIf {
		return logger.FormatErrMsg(&token.Loc, "`else` may be only inside if-do-end block")
	}
	if block.Tok.Typ != lexer.TokenKeyword {
		return logger.FormatErrMsg(&token.Loc, "Only keywords may form blocks, but not `%s`. Probably bug in lexer", block.Tok.Text)
	}
	block_start_kw := block.Tok.Value.(lexer.KeywordType)

	addr := c.getCurrentAddr()

	switch block_start_kw {
	case lexer.KeywordDo:
		c.Ops[block.Addr].Operand = addr - block.Addr + 1
	case lexer.KeywordIf, lexer.KeywordElse, lexer.KeywordEnd, lexer.KeywordWhile:
		return logger.FormatErrMsg(&token.Loc, "`else` may only come after `do` in `if`-block, but got `%s`", block.Tok.Text)
	default:
		return logger.FormatErrMsg(&token.Loc, "Unhandled block start processing")
	}

	c.Blocks.Push(NewBlock(addr, token, block.Typ))
	c.pushOps(scope_name, Op{OpToken: *token, Typ: OpElse})

	return nil
}

func (c *Compiler) compileWhileBlock(token *lexer.Token, th *lexer.TokenHolder, ctx *CompileTimeContext, scope_name string) error {
	c.Blocks.Push(NewBlock(c.getCurrentAddr(), token, lexer.KeywordWhile))
	c.pushOps(scope_name, Op{OpToken: *token, Typ: OpWhile})
	return c.compile(th, ctx, scope_name)
}

func (c *Compiler) compileDoBlock(token *lexer.Token, th *lexer.TokenHolder, ctx *CompileTimeContext, scope_name string) error {
	if c.Blocks.Size() == 0 {
		return logger.FormatErrMsg(&token.Loc, "Unexpected `do` found")
	}
	block := c.Blocks.Pop().(*Block)
	if block.Tok.Typ != lexer.TokenKeyword {
		return logger.FormatErrMsg(&token.Loc, "Only keywords may form c.Blocks, but got `%s`", block.Tok.Text)
	}

	kw := block.Tok.Value.(lexer.KeywordType)
	if kw != lexer.KeywordWhile && kw != lexer.KeywordIf {
		return logger.FormatErrMsg(&token.Loc, "`do` may come only in `while`, `if` or `func` block, but not `%s`", block.Tok.Text)
	}

	do_addr := c.getCurrentAddr()
	c.Blocks.Push(NewBlock(do_addr, token, block.Typ))
	c.pushOps(scope_name, Op{OpToken: *token, Typ: OpDo, Operand: block.Addr - do_addr})
	return nil
}

func (c *Compiler) compileJumpKeyword(token *lexer.Token, op_type OpType, kw lexer.KeywordType, scope_name string) error {
	if kw != lexer.KeywordContinue && kw != lexer.KeywordBreak {
		return logger.FormatErrMsg(&token.Loc, "Only `break` and `continue` are supported as jumps, but got `%s`", lexer.KeywordName[kw])
	}

	if c.Blocks.Size() == 0 {
		return logger.FormatErrMsg(&token.Loc, "Unexpected `%s` found", token.Text)
	}

	var i int
	for i = len(c.Blocks.Data) - 1; i >= 0; i-- {
		cur_block := c.Blocks.Data[i].(*Block)

		if cur_block.Tok.Typ == lexer.TokenKeyword && cur_block.Tok.Value.(lexer.KeywordType) == lexer.KeywordDo {
			if cur_block.Typ == lexer.KeywordWhile {
				cur_block.Jumps = append(cur_block.Jumps, Jump{Keyword: kw, Addr: c.getCurrentAddr()})
				break
			}
		}
	}
	if i < 0 {
		return logger.FormatErrMsg(&token.Loc, "`%s` should be inside while-loop, but it doesn't", token.Text)
	}

	c.pushOps(scope_name, Op{OpToken: *token, Typ: op_type})
	return nil
}

func (c *Compiler) compileBreakKeyword(token *lexer.Token, scope_name string) error {
	return c.compileJumpKeyword(token, OpBreak, lexer.KeywordBreak, scope_name)
}

func (c *Compiler) compileContinueKeyword(token *lexer.Token, scope_name string) error {
	return c.compileJumpKeyword(token, OpContinue, lexer.KeywordContinue, scope_name)
}

func (c *Compiler) compileEndKeyword(token *lexer.Token, ctx *CompileTimeContext, scope_name string) error {
	if c.Blocks.Size() == 0 {
		return logger.FormatErrMsg(&token.Loc, "Unexpected `end` found")
	}
	block := c.Blocks.Pop().(*Block)

	if block.Typ != lexer.KeywordFunc && block.Typ != lexer.KeywordWhile && block.Typ != lexer.KeywordIf {
		return logger.FormatErrMsg(&token.Loc, "`end` should close only `if`, `while` or `func` blocks, but not %s", block.Tok.Text)
	}

	if block.Tok.Typ != lexer.TokenKeyword {
		return logger.FormatErrMsg(&token.Loc, "Only keywords may form blocks, but not `%s`. Probably bug in lexer", block.Tok.Text)
	}
	block_start_kw := block.Tok.Value.(lexer.KeywordType)

	op := Op{OpToken: *token, Operand: types.IntType(1)}
	addr := c.getCurrentAddr()

	switch block_start_kw {
	case lexer.KeywordIf, lexer.KeywordWhile:
		return logger.FormatErrMsg(&block.Tok.Loc, "`%s` block must contain `do` before `end`", lexer.KeywordName[block_start_kw])
	case lexer.KeywordElse:	// if-do-else-end
		c.Ops[block.Addr].Operand = addr - block.Addr + 1
		op.Typ = OpEnd
		c.pushOps(scope_name, op)
	case lexer.KeywordDo:
		switch block.Typ {
		case lexer.KeywordWhile:
			do_while_addr_diff := c.Ops[block.Addr].Operand.(types.IntType)
			for _, jump := range block.Jumps {
				switch jump.Keyword {
				case lexer.KeywordBreak:
					c.Ops[jump.Addr].Operand = addr - jump.Addr + 1 // break -> end + 1
				case lexer.KeywordContinue:
					c.Ops[jump.Addr].Operand = block.Addr + do_while_addr_diff - jump.Addr // continue -> while
				default:
					return logger.FormatErrMsg(&block.Tok.Loc, "Unhandled jump-keyword: `%s`", lexer.KeywordName[jump.Keyword])
				}
			}
			op.Operand = block.Addr + do_while_addr_diff - addr
			c.Ops[block.Addr].Operand = addr - block.Addr + 1
			op.Typ = OpEnd
			c.pushOps(scope_name, op)
		case lexer.KeywordIf: // if-do-end
			c.Ops[block.Addr].Operand = addr - block.Addr + 1
			op.Typ = OpEnd
			c.pushOps(scope_name, op)
		default:
			return logger.FormatErrMsg(&token.Loc, "Unhandled block type while compiling `end` keyword")
		}

	case lexer.KeywordEnd:
		return logger.FormatErrMsg(&token.Loc, "`end` may only close `if-else` or `while-do` blocks, but got `%s`", block.Tok.Text)
	case lexer.KeywordBreak, lexer.KeywordContinue:
		return logger.FormatErrMsg(&block.Tok.Loc, "`%s` keyword shouldn't be in blocks stack", lexer.KeywordName[block_start_kw])
	case lexer.KeywordFunc:
		func_end_op := Op{
			OpToken: *token, Typ: OpFuncEnd, Operand: scope_name, Data: scope_name,
		}
		c.pushOps(scope_name, func_end_op)
	default:
		return logger.FormatErrMsg(&token.Loc, "Unhandled block start processing (processEndKeyword)")
	}
	return nil
}

func (c *Compiler) compileNamedBlock(token *lexer.Token, th *lexer.TokenHolder, ctx *CompileTimeContext, scope_name string, typ string) (name_token lexer.Token, value types.IntType, err error) {

	if th.Empty() {
		err = logger.FormatErrMsg(&token.Loc, "Expected `%s` name, but got nothing", typ)
		return
	}

	scope := ctx.Scopes[scope_name]

	name_token = *th.GetNextToken()

	if name_token.Typ != lexer.TokenWord {
		err = logger.FormatErrMsg(&token.Loc, "Expected `%s` name to be a word, but got `%s`", typ, name_token.Text)
		return
	}
	defined_token, exists := scope.Names[name_token.Text]
	if exists {
		msg := logger.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
		err = logger.FormatErrMsg(&name_token.Loc, "Redefinition of name `%s` (%s)", name_token.Text, msg)
		return
	}

	defined_token, exists = ctx.GlobalScope().Names[name_token.Text]
	if exists {
		_, func_exists := ctx.Funcs[name_token.Text]
		if func_exists {
			msg := logger.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
			err = logger.FormatErrMsg(&name_token.Loc, "Redefinition of function `%s` (%s)", name_token.Text, msg)
			return
		}

		msg := logger.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
		err = logger.FormatErrMsg(&name_token.Loc, "Redefinition of global name `%s` (%s)", name_token.Text, msg)
		return
	}

	scope.Names[name_token.Text] = name_token

	const_th := lexer.NewTokenHolder()
	for {
		if th.Empty() {
			err = logger.FormatErrMsg(&token.Loc, "Unexpected end while processing `%s` block", typ)
			return
		}

		btok := *th.GetNextToken()
		if btok.Typ == lexer.TokenKeyword && btok.Value.(lexer.KeywordType) == lexer.KeywordEnd {
			break
		}

		const_th.AppendToken(btok)
	}

	value, err = c.constEval(&name_token, const_th, ctx, scope)
	return
}

func (c *Compiler) compileFuncDef(token *lexer.Token, th *lexer.TokenHolder, ctx *CompileTimeContext) (name_token lexer.Token, func_name string, err error) {
	if th.Empty() {
		err = logger.FormatErrMsg(&token.Loc, "Expected `%s` name, but got nothing", token.Text)
		return
	}

	if token.Typ == lexer.TokenKeyword {
		kw_type := token.Value.(lexer.KeywordType)
		if kw_type != lexer.KeywordFunc {
			err = logger.FormatErrMsg(&token.Loc, "Expected `func` keyword, but found %s", token.Text)
			return
		}
	}

	if token.Typ == lexer.TokenKeyword && token.Value.(lexer.KeywordType) != lexer.KeywordFunc {
		err = logger.FormatErrMsg(&token.Loc, "Expected `func` keyword, but found %s", token.Text)
		return
	}

	name_token = *th.GetNextToken()

	if name_token.Typ != lexer.TokenWord {
		err = logger.FormatErrMsg(&token.Loc, "Expected `%s` name to be a word, but got `%s`", token.Text, name_token.Text)
		return
	}

	defined_token, exists := ctx.GlobalScope().Names[name_token.Text]
	if exists {
		msg := logger.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
		err = logger.FormatErrMsg(&name_token.Loc, "Redefinition of global name `%s` in function definition (%s)", name_token.Text, msg)
		return
	}

	func_name = name_token.Text
	ctx.GlobalScope().Names[func_name] = name_token

	do_token := *th.GetNextToken()

	if do_token.Typ != lexer.TokenKeyword {
		err = logger.FormatErrMsg(&token.Loc, "Expected `do` to start the function name, but got `%s`", token.Text)
		return
	}
	if do_token.Value.(lexer.KeywordType) != lexer.KeywordDo {
		err = logger.FormatErrMsg(&token.Loc, "Expected keyword `do` to start the function name, but got `%s`", token.Text)
		return
	}

	return
}

func (c *Compiler) compileFunc(token *lexer.Token, th *lexer.TokenHolder, ctx *CompileTimeContext, scope_name string) error {

	if scope_name != GlobalScopeName {
		return logger.FormatErrMsg(&token.Loc, "Cannot define functions inside a function %s", scope_name)
	}
	func_token, func_name, err := c.compileFuncDef(token, th, ctx)
	if err != nil {
		return err
	}

	new_scope := NewScope(func_name)
	new_scope.Names[func_name] = func_token
	ctx.Scopes[func_name] = new_scope

	func_addr := c.getCurrentAddr()
	c.Blocks.Push(NewBlock(func_addr, token, lexer.KeywordFunc))

	c.pushOps(func_name, Op{OpToken: *token, Typ: OpFuncBegin, Data: func_name})
	if err = c.compile(th, ctx, func_name); err != nil {
		return err
	}

	ctx.Funcs[func_name] = Function{Addr: func_addr}

	c.Ops[func_addr].Operand = new_scope.MemSize            // OpFuncBegin $MEM - allocates   $MEM bytes in RAM
	c.Ops[c.getCurrentAddr()-1].Operand = new_scope.MemSize // OpFuncEnd $MEM   - deallocates $MEM bytes in RAM

	return nil
}

func (c *Compiler) constEval(token *lexer.Token, th *lexer.TokenHolder, ctx *CompileTimeContext, scope *Scope) (value types.IntType, err error) {
	const_stack := &utils.Stack{}
	for !th.Empty() {
		token := th.GetNextToken()

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
						err = logger.FormatErrMsg(&token.Loc, "Division by zero")
						return
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a / b)
				case lexer.IntrinsicMod:
					b := const_stack.Pop().(types.IntType)
					if b == 0 {
						err = logger.FormatErrMsg(&token.Loc, "Division by zero")
						return
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a % b)
				case lexer.IntrinsicShl:
					b := const_stack.Pop().(types.IntType)
					if b < 0 {
						err = logger.FormatErrMsg(&token.Loc, "Negative shift amount in `<<`: %d", b)
						return
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a << b)
				case lexer.IntrinsicShr:
					b := const_stack.Pop().(types.IntType)
					if b < 0 {
						err = logger.FormatErrMsg(&token.Loc, "Negative shift amount in `>>`: %d", b)
						return
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a >> b)
				case lexer.IntrinsicOffset:
					off := const_stack.Pop().(types.IntType)
					const_stack.Push(ctx.Offset)
					ctx.Offset += off
				case lexer.IntrinsicReset:
					const_stack.Push(ctx.Offset)
					ctx.Offset = 0
				default:
					err = logger.FormatErrMsg(
						&token.Loc,
						"Unexpected intrinsic in const-block compile-time "+
							"evaluation: `%s`. Supported: [+, -, *, /, %%, >>, <<, offset, reset]", token.Text,
					)
					return
				}
				continue
			}

			val, scope := ctx.GetConst(token.Text, scope.ScopeName)
			if scope != ScopeUnknown {
				const_stack.Push(val)
				continue
			}

			err = logger.FormatErrMsg(&token.Loc, "Unsupported word in compile-time const-block evaluation: `%s`", token.Text)
			return
		default:
			err = logger.FormatErrMsg(&token.Loc, "Unsupported token in compile-time const-block evaluation: `%s`", token.Text)
			return
		}
	}

	if const_stack.Size() > 1 {
		err = logger.FormatErrMsg(&token.Loc, "Unhandled data in compile-time const-block evaluation stack")
		return
	}

	value, _ = const_stack.Pop().(types.IntType)
	return
}

func (c *Compiler) compile(th *lexer.TokenHolder, ctx *CompileTimeContext, scope_name string) error {

	scope, exists := ctx.Scopes[scope_name]
	if !exists {
		return logger.FormatErrMsg(nil, "No such scope: `%s`", scope_name)
	}

	var token *lexer.Token
	for !th.Empty() {

		token = th.GetNextToken()

		switch token.Typ {
		case lexer.TokenInt:
			if err := c.compileTokenInt(token, scope_name); err != nil {
				return err
			}
		case lexer.TokenString:
			if err := c.compileTokenString(token, ctx, scope_name); err != nil {
				return err
			}
		case lexer.TokenChar:
			if err := c.compileTokenChar(token, scope_name); err != nil {
				return err
			}
		case lexer.TokenBool:
			if err := c.compileTokenBool(token, scope_name); err != nil {
				return err
			}
		case lexer.TokenWord:
			name := token.Text

			intrinsic, exists := lexer.WordToIntrinsic[name]
			if exists {
				if err := c.compileTokenIntrinsic(token, scope_name, intrinsic); err != nil {
					return err
				}
				continue
			}

			val, exists := ctx.GetLocalConst(name, scope_name)
			if exists {
				if err := c.compileLocalConst(token, val, scope_name); err != nil {
					return err
				}
				continue
			}

			_, exists = ctx.GetLocalAlloc(name, scope_name)
			if exists {
				if err := c.compileLocalAlloc(token, scope); err != nil {
					return err
				}
				continue
			}

			val, exists = ctx.GetGlobalConst(name)
			if exists {
				if err := c.compileGlobalConst(token, val, scope_name); err != nil {
					return err
				}
				continue
			}

			_, exists = ctx.GetGlobalAlloc(name)
			if exists {
				if err := c.compileGlobalAlloc(token, scope); err != nil {
					return err
				}
				continue
			}

			function, exists := ctx.Funcs[name]
			if exists {
				if err := c.compileFuncCall(token, &function, scope_name); err != nil {
					return err
				}
				continue
			}

			return logger.FormatErrMsg(&token.Loc, "Unknown word: `%s`", token.Text)

		case lexer.TokenKeyword:
			kw_type := token.Value.(lexer.KeywordType)
			scope := ctx.Scopes[scope_name]

			switch kw_type {
			case lexer.KeywordIf:
				if err := c.compileIfBlock(token, th, ctx, scope_name); err != nil {
					return err
				}
			case lexer.KeywordElse:
				if err := c.compileElseBlock(token, th, ctx, scope_name); err != nil {
					return err
				}
			case lexer.KeywordEnd:
				return c.compileEndKeyword(token, ctx, scope_name) // return from compile() after processing `end`
			case lexer.KeywordWhile:
				if err := c.compileWhileBlock(token, th, ctx, scope_name); err != nil {
					return err
				}
			case lexer.KeywordDo:
				if err := c.compileDoBlock(token, th, ctx, scope_name); err != nil {
					return err
				}
			case lexer.KeywordBreak:
				if err := c.compileBreakKeyword(token, scope_name); err != nil {
					return err
				}
			case lexer.KeywordContinue:
				if err := c.compileContinueKeyword(token, scope_name); err != nil {
					return err
				}

			case lexer.KeywordConst:
				tok, const_value, err := c.compileNamedBlock(token, th, ctx, scope_name, token.Text)
				if err != nil {
					return err
				}
				scope.Consts[tok.Text] = const_value
			case lexer.KeywordAlloc:
				tok, alloc_size, err := c.compileNamedBlock(token, th, ctx, scope_name, token.Text)
				if err != nil {
					return err
				}
				if alloc_size < 0 {
					return logger.FormatErrMsg(&tok.Loc, "Negative size for `alloc` block: %d", alloc_size)
				}
				scope.Allocs[tok.Text] = Allocation{
					Offset: scope.MemSize, Size: alloc_size,
				}
				scope.MemSize += alloc_size

			case lexer.KeywordFunc:
				if err := c.compileFunc(token, th, ctx, scope_name); err != nil {
					return err
				}

			case lexer.KeywordInclude:
				return logger.FormatErrMsg(&token.Loc, "Include keyword should not appear in here, probably there is a bug in a lexer")
			default:
				return logger.FormatErrMsg(&token.Loc, "Unhandled KewordType handling: `%s`", token.Text)
			}
		default:
			return logger.FormatErrMsg(&token.Loc, "Unhandled token: `%s`\n", token.Text)
		}
	}
	return nil
}

func (c *Compiler) CompileTokens(th *lexer.TokenHolder, ctx *CompileTimeContext) error {
	th.Reset()
	if err := c.compile(th, ctx, GlobalScopeName); err != nil {
		return err
	}

	if !th.Empty() {
		return logger.FormatErrMsg(nil, "[ERROR] No all tokens were processed")
	}

	if len(c.Blocks.Data) > 0 {
		top := c.Blocks.Data[len(c.Blocks.Data)-1].(*Block)
		return logger.FormatErrMsg(&top.Tok.Loc, "Unclosed `%s`-block", top.Tok.Text)
	}

	_, exists := ctx.Funcs["main"]
	if !exists {
		return logger.FormatErrMsg(nil, "No entry point found (function `main` was not defined)")
	}

	return nil
}
