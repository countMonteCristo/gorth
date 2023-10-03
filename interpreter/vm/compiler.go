package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
)

type Compiler struct {
	Blocks utils.Stack
	Ops    []Op
}

func NewCompiler() *Compiler {
	return &Compiler{Blocks: utils.Stack{}, Ops: make([]Op, 0)}
}

func (c *Compiler) getCurrentAddr() (addr types.IntType) {
	return types.IntType(len(c.Ops))
}

func (c *Compiler) compileTokenInt(token *lexer.Token) error {
	c.Ops = append(c.Ops, Op{Typ: OpPushInt, Operand: token.Value.(types.IntType), OpToken: *token})
	return nil
}

func (c *Compiler) compileTokenString(token *lexer.Token, ctx *Context) error {
	literal := token.Value.(string)
	literal_addr, exists := ctx.Memory.StringsMap[literal]
	if !exists {
		return lexer.FormatErrMsg(&token.Loc, "Unknown string literal at compile-time: `%s`", literal)
	}
	c.Ops = append(c.Ops,
		Op{Typ: OpPushInt, Operand: literal_addr, OpToken: *token},
		Op{Typ: OpPushInt, Operand: types.IntType(len(literal)), OpToken: *token},
	)
	return nil
}

func (c *Compiler) compileTokenChar(token *lexer.Token) error {
	c.Ops = append(c.Ops, Op{Typ: OpPushInt, Operand: token.Value.(types.IntType), OpToken: *token})
	return nil
}

func (c *Compiler) compileTokenBool(token *lexer.Token) error {
	c.Ops = append(c.Ops, Op{Typ: OpPushBool, Operand: token.Value.(types.BoolType), OpToken: *token})
	return nil
}

func (c *Compiler) compileTokenIntrinsic(token *lexer.Token, intrinsic lexer.IntrinsicType) error {
	c.Ops = append(c.Ops, Op{Typ: OpIntrinsic, Operand: intrinsic, OpToken: *token})
	return nil
}

func (c *Compiler) compileLocalConst(token *lexer.Token, val types.IntType) error {
	c.Ops = append(c.Ops, Op{Typ: OpPushInt, Operand: val, OpToken: *token})
	return nil
}

func (c *Compiler) compileGlobalConst(token *lexer.Token, val types.IntType) error {
	c.Ops = append(c.Ops, Op{Typ: OpPushInt, Operand: val, OpToken: *token})
	return nil
}

func (c *Compiler) compileLocalAlloc(token *lexer.Token) error {
	c.Ops = append(c.Ops, Op{Typ: OpPushAlloc, Operand: token.Text, OpToken: *token})
	return nil
}

func (c *Compiler) compileGlobalAlloc(token *lexer.Token) error {
	c.Ops = append(c.Ops, Op{Typ: OpPushAlloc, Operand: token.Text, OpToken: *token})
	return nil
}

func (c *Compiler) compileFuncCall(token *lexer.Token, f *Function) error {
	c.Ops = append(c.Ops, Op{Typ: OpCall, Operand: f.Addr - c.getCurrentAddr(), OpToken: *token})
	return nil
}

func (c *Compiler) compileIfBlock(token *lexer.Token, tokens *[]lexer.Token, ctx *Context, scope_name string) error {
	c.Blocks.Push(NewBlock(c.getCurrentAddr(), token))

	c.Ops = append(c.Ops, Op{OpToken: *token, Typ: OpIf})
	return c.compile(tokens, ctx, scope_name)
}

func (c *Compiler) compileElseBlock(token *lexer.Token, tokens *[]lexer.Token, ctx *Context, scope_name string) error {

	if c.Blocks.Size() == 0 {
		return lexer.FormatErrMsg(&token.Loc, "Unexpected `end` found")
	}
	block := c.Blocks.Pop().(*Block)
	if block.Tok.Typ != lexer.TokenKeyword {
		return lexer.FormatErrMsg(&token.Loc, "Only keywords may form blocks, but not `%s`. Probably bug in lexer", block.Tok.Text)
	}
	block_start_kw := block.Tok.Value.(lexer.KeywordType)

	addr := c.getCurrentAddr()

	switch block_start_kw {
	case lexer.KeywordIf:
		c.Ops[block.Addr].Operand = addr - block.Addr + 1
	case lexer.KeywordElse, lexer.KeywordEnd, lexer.KeywordWhile:
		return lexer.FormatErrMsg(&token.Loc, "`else` may only come after `if` block, but got `%s`", block.Tok.Text)
	default:
		return lexer.FormatErrMsg(&token.Loc, "Unhandled block start processing")
	}

	c.Blocks.Push(NewBlock(addr, token))
	c.Ops = append(c.Ops, Op{OpToken: *token, Typ: OpElse})

	return nil
}

func (c *Compiler) compileWhileBlock(token *lexer.Token, tokens *[]lexer.Token, ctx *Context, scope_name string) error {
	c.Blocks.Push(NewBlock(c.getCurrentAddr(), token))
	c.Ops = append(c.Ops, Op{OpToken: *token, Typ: OpWhile})
	return c.compile(tokens, ctx, scope_name)
}

func (c *Compiler) compileDoBlock(token *lexer.Token, tokens *[]lexer.Token, ctx *Context, scope_name string) error {
	if c.Blocks.Size() == 0 {
		return lexer.FormatErrMsg(&token.Loc, "Unexpected `do` found")
	}
	block := c.Blocks.Pop().(*Block)
	if block.Tok.Typ != lexer.TokenKeyword {
		return lexer.FormatErrMsg(&token.Loc, "Only keywords may form c.Blocks, but got `%s`", block.Tok.Text)
	}
	if block.Tok.Value.(lexer.KeywordType) != lexer.KeywordWhile {
		return lexer.FormatErrMsg(&token.Loc, "`do` may come only inside `while` block, but not `%s`", block.Tok.Text)
	}

	do_addr := c.getCurrentAddr()
	c.Blocks.Push(NewBlock(do_addr, token))
	c.Ops = append(c.Ops, Op{OpToken: *token, Typ: OpDo, Operand: block.Addr - do_addr})
	return nil
}

func (c *Compiler) compileJumpKeyword(token *lexer.Token, op_type OpType) error {
	if c.Blocks.Size() == 0 {
		return lexer.FormatErrMsg(&token.Loc, "Unexpected `%s` found", token.Text)
	}

	var i int
	for i = len(c.Blocks.Data) - 1; i >= 0; i-- {
		cur_block := c.Blocks.Data[i].(*Block)

		if cur_block.Tok.Typ == lexer.TokenKeyword && cur_block.Tok.Value.(lexer.KeywordType) == lexer.KeywordDo {
			cur_block.Jumps = append(cur_block.Jumps, Jump{Keyword: lexer.KeywordBreak, Addr: c.getCurrentAddr()})
			break
		}
	}
	if i < 0 {
		return lexer.FormatErrMsg(&token.Loc, "`%s` should be inside while-loop, but it doesn't", token.Text)
	}

	c.Ops = append(c.Ops, Op{OpToken: *token, Typ: op_type})
	return nil
}

func (c *Compiler) compileBreakKeyword(token *lexer.Token) error {
	return c.compileJumpKeyword(token, OpBreak)
}

func (c *Compiler) compileContinueKeyword(token *lexer.Token) error {
	return c.compileJumpKeyword(token, OpContinue)
}

func (c *Compiler) compileEndKeyword(token *lexer.Token, ctx *Context, scope_name string) error {
	if c.Blocks.Size() == 0 {
		return lexer.FormatErrMsg(&token.Loc, "Unexpected `end` found")
	}
	block := c.Blocks.Pop().(*Block)
	if block.Tok.Typ != lexer.TokenKeyword {
		return lexer.FormatErrMsg(&token.Loc, "Only keywords may form blocks, but not `%s`. Probably bug in lexer", block.Tok.Text)
	}
	block_start_kw := block.Tok.Value.(lexer.KeywordType)

	op := Op{OpToken: *token, Operand: types.IntType(1)}
	addr := c.getCurrentAddr()

	switch block_start_kw {
	case lexer.KeywordIf:
		c.Ops[block.Addr].Operand = addr - block.Addr + 1
		op.Typ = OpEnd
		c.Ops = append(c.Ops, op)
	case lexer.KeywordElse:
		c.Ops[block.Addr].Operand = addr - block.Addr + 1
		op.Typ = OpEnd
		c.Ops = append(c.Ops, op)
	case lexer.KeywordWhile:
		return lexer.FormatErrMsg(&block.Tok.Loc, "`while` block must contain `do` before `end`")
	case lexer.KeywordDo:
		do_while_addr_diff := c.Ops[block.Addr].Operand.(types.IntType)
		for _, jump := range block.Jumps {
			switch jump.Keyword {
			case lexer.KeywordBreak:
				c.Ops[jump.Addr].Operand = addr - jump.Addr + 1 // break -> end + 1
			case lexer.KeywordContinue:
				c.Ops[jump.Addr].Operand = block.Addr + do_while_addr_diff - jump.Addr // continue -> while
			default:
				return lexer.FormatErrMsg(&block.Tok.Loc, "Unhandled jump-keyword: `%s`", lexer.KeywordName[jump.Keyword])
			}
		}
		op.Operand = block.Addr + do_while_addr_diff - addr
		c.Ops[block.Addr].Operand = addr - block.Addr + 1
		op.Typ = OpEnd
		c.Ops = append(c.Ops, op)
	case lexer.KeywordEnd:
		return lexer.FormatErrMsg(&token.Loc, "`end` may only close `if-else` or `while-do` blocks, but got `%s`", block.Tok.Text)
	case lexer.KeywordBreak:
		return lexer.FormatErrMsg(&block.Tok.Loc, "`break` keyword shouldn't be in blocks stack")
	case lexer.KeywordContinue:
		return lexer.FormatErrMsg(&block.Tok.Loc, "`continue` keyword shouldn't be in blocks stack")
	case lexer.KeywordFunc:
		func_end_op := Op{
			OpToken: *token, Typ: OpFuncEnd, Operand: scope_name,
		}
		c.Ops = append(c.Ops, func_end_op)
	default:
		return lexer.FormatErrMsg(&token.Loc, "Unhandled block start processing")
	}
	return nil
}

func (c *Compiler) compileNamedBlock(token *lexer.Token, tokens *[]lexer.Token, ctx *Context, scope_name string, typ string) (name_token lexer.Token, value types.IntType, err error) {

	if len(*tokens) == 0 {
		err = lexer.FormatErrMsg(&token.Loc, "Expected `%s` name, but got nothing", typ)
		return
	}

	scope := ctx.Scopes[scope_name]

	name_token, *tokens = (*tokens)[0], (*tokens)[1:]

	if name_token.Typ != lexer.TokenWord {
		err = lexer.FormatErrMsg(&token.Loc, "Expected `%s` name to be a word, but got `%s`", typ, name_token.Text)
		return
	}
	defined_token, exists := scope.Names[name_token.Text]
	if exists {
		msg := lexer.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
		err = lexer.FormatErrMsg(&name_token.Loc, "Redefinition of name `%s` (%s)", name_token.Text, msg)
		return
	}

	defined_token, exists = ctx.GlobalScope().Names[name_token.Text]
	if exists {
		_, func_exists := ctx.Funcs[name_token.Text]
		if func_exists {
			msg := lexer.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
			err = lexer.FormatErrMsg(&name_token.Loc, "Redefinition of function `%s` (%s)", name_token.Text, msg)
			return
		}

		msg := lexer.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
		err = lexer.FormatErrMsg(&name_token.Loc, "Redefinition of global name `%s` (%s)", name_token.Text, msg)
		return
	}

	scope.Names[name_token.Text] = name_token

	const_block := make([]lexer.Token, 0)
	for {
		if len(*tokens) == 0 {
			err = lexer.FormatErrMsg(&token.Loc, "Unexpected end while processing `%s` block", typ)
			return
		}
		var btok lexer.Token
		btok, *tokens = (*tokens)[0], (*tokens)[1:]

		if btok.Typ == lexer.TokenKeyword && btok.Value.(lexer.KeywordType) == lexer.KeywordEnd {
			break
		}

		const_block = append(const_block, btok)
	}

	value, err = c.constEval(&name_token, &const_block, ctx, scope)
	return
}

func (c *Compiler) compileFuncDef(token *lexer.Token, tokens *[]lexer.Token, ctx *Context) (name_token lexer.Token, func_name string, err error) {
	if len(*tokens) == 0 {
		err = lexer.FormatErrMsg(&token.Loc, "Expected `%s` name, but got nothing", token.Text)
		return
	}

	if token.Typ == lexer.TokenKeyword {
		kw_type := token.Value.(lexer.KeywordType)
		if kw_type != lexer.KeywordFunc {
			err = lexer.FormatErrMsg(&token.Loc, "Expected `func` keyword, but found %s", token.Text)
			return
		}
	}

	if token.Typ == lexer.TokenKeyword && token.Value.(lexer.KeywordType) != lexer.KeywordFunc {
		err = lexer.FormatErrMsg(&token.Loc, "Expected `func` keyword, but found %s", token.Text)
		return
	}

	name_token, *tokens = (*tokens)[0], (*tokens)[1:]

	if name_token.Typ != lexer.TokenWord {
		err = lexer.FormatErrMsg(&token.Loc, "Expected `%s` name to be a word, but got `%s`", token.Text, name_token.Text)
		return
	}

	defined_token, exists := ctx.GlobalScope().Names[name_token.Text]
	if exists {
		msg := lexer.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
		err = lexer.FormatErrMsg(&name_token.Loc, "Redefinition of global name `%s` in function definition (%s)", name_token.Text, msg)
		return
	}

	func_name = name_token.Text
	ctx.GlobalScope().Names[func_name] = name_token

	var do_token lexer.Token
	do_token, *tokens = (*tokens)[0], (*tokens)[1:]

	if do_token.Typ != lexer.TokenKeyword {
		lexer.FormatErrMsg(&token.Loc, "Expected `do` to start the function name, but got `%s`", token.Text)
	}
	if do_token.Value.(lexer.KeywordType) != lexer.KeywordDo {
		lexer.FormatErrMsg(&token.Loc, "Expected keyword `do` to start the function name, but got `%s`", token.Text)
	}

	return
}

func (c *Compiler) compileFunc(token *lexer.Token, tokens *[]lexer.Token, ctx *Context, scope_name string) error {

	if scope_name != GlobalScopeName {
		return lexer.FormatErrMsg(&token.Loc, "Cannot define functions inside a function %s", scope_name)
	}
	func_token, func_name, err := c.compileFuncDef(token, tokens, ctx)
	if err != nil {
		return err
	}

	new_scope := NewScope(func_name)
	new_scope.Names[func_name] = func_token
	ctx.Scopes[func_name] = new_scope

	func_addr := c.getCurrentAddr()
	c.Blocks.Push(NewBlock(func_addr, token))

	c.Ops = append(c.Ops, Op{OpToken: *token, Typ: OpFuncBegin, Operand: func_name})
	if err = c.compile(tokens, ctx, func_name); err != nil {
		return err
	}

	ctx.Funcs[func_name] = Function{Addr: func_addr}
	return nil
}

func (c *Compiler) constEval(token *lexer.Token, tokens *[]lexer.Token, ctx *Context, scope *Scope) (value types.IntType, err error) {
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
						lexer.FormatErrMsg(&token.Loc, "Division by zero")
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a / b)
				case lexer.IntrinsicMod:
					b := const_stack.Pop().(types.IntType)
					if b == 0 {
						lexer.FormatErrMsg(&token.Loc, "Division by zero")
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a % b)
				case lexer.IntrinsicShl:
					b := const_stack.Pop().(types.IntType)
					if b < 0 {
						err = lexer.FormatErrMsg(&token.Loc, "Negative shift amount in `<<`: %d", b)
						return
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a << b)
				case lexer.IntrinsicShr:
					b := const_stack.Pop().(types.IntType)
					if b < 0 {
						err = lexer.FormatErrMsg(&token.Loc, "Negative shift amount in `>>`: %d", b)
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
					err = lexer.FormatErrMsg(
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

			err = lexer.FormatErrMsg(&token.Loc, "Unsupported word in compile-time const-block evaluation: `%s`", token.Text)
			return
		default:
			err = lexer.FormatErrMsg(&token.Loc, "Unsupported token in compile-time const-block evaluation: `%s`", token.Text)
			return
		}
	}

	if const_stack.Size() > 1 {
		lexer.FormatErrMsg(&token.Loc, "Unhandled data in compile-time const-block evaluation stack")
	}

	value, _ = const_stack.Pop().(types.IntType)
	return
}

func (c *Compiler) compile(tokens *[]lexer.Token, ctx *Context, scope_name string) error {

	var token lexer.Token
	for len(*tokens) > 0 {

		token, *tokens = (*tokens)[0], (*tokens)[1:]

		switch token.Typ {
		case lexer.TokenInt:
			if err := c.compileTokenInt(&token); err != nil {
				return err
			}
		case lexer.TokenString:
			if err := c.compileTokenString(&token, ctx); err != nil {
				return err
			}
		case lexer.TokenChar:
			if err := c.compileTokenChar(&token); err != nil {
				return err
			}
		case lexer.TokenBool:
			if err := c.compileTokenBool(&token); err != nil {
				return err
			}
		case lexer.TokenWord:
			name := token.Text

			intrinsic, exists := lexer.WordToIntrinsic[name]
			if exists {
				if err := c.compileTokenIntrinsic(&token, intrinsic); err != nil {
					return err
				}
				continue
			}

			val, exists := ctx.GetLocalConst(name, scope_name)
			if exists {
				if err := c.compileLocalConst(&token, val); err != nil {
					return err
				}
				continue
			}

			_, exists = ctx.GetLocalAlloc(name, scope_name)
			if exists {
				if err := c.compileLocalAlloc(&token); err != nil {
					return err
				}
				continue
			}

			val, exists = ctx.GetGlobalConst(name)
			if exists {
				if err := c.compileGlobalConst(&token, val); err != nil {
					return err
				}
				continue
			}

			_, exists = ctx.GetGlobalAlloc(name)
			if exists {
				if err := c.compileGlobalAlloc(&token); err != nil {
					return err
				}
				continue
			}

			function, exists := ctx.Funcs[name]
			if exists {
				if err := c.compileFuncCall(&token, &function); err != nil {
					return err
				}
				continue
			}

			return lexer.FormatErrMsg(&token.Loc, "Unknown word: `%s`", token.Text)

		case lexer.TokenKeyword:
			kw_type := token.Value.(lexer.KeywordType)
			scope := ctx.Scopes[scope_name]

			switch kw_type {
			case lexer.KeywordIf:
				if err := c.compileIfBlock(&token, tokens, ctx, scope_name); err != nil {
					return err
				}
			case lexer.KeywordElse:
				if err := c.compileElseBlock(&token, tokens, ctx, scope_name); err != nil {
					return err
				}
			case lexer.KeywordEnd:
				return c.compileEndKeyword(&token, ctx, scope_name) // return from compile() after processing `end`
			case lexer.KeywordWhile:
				if err := c.compileWhileBlock(&token, tokens, ctx, scope_name); err != nil {
					return err
				}
			case lexer.KeywordDo:
				if err := c.compileDoBlock(&token, tokens, ctx, scope_name); err != nil {
					return err
				}
			case lexer.KeywordBreak:
				if err := c.compileBreakKeyword(&token); err != nil {
					return err
				}
			case lexer.KeywordContinue:
				if err := c.compileContinueKeyword(&token); err != nil {
					return err
				}

			case lexer.KeywordConst:
				tok, const_value, err := c.compileNamedBlock(&token, tokens, ctx, scope_name, token.Text)
				if err != nil {
					return err
				}
				scope.Consts[tok.Text] = const_value
			case lexer.KeywordAlloc:
				tok, alloc_size, err := c.compileNamedBlock(&token, tokens, ctx, scope_name, token.Text)
				if err != nil {
					return err
				}
				if alloc_size < 0 {
					return lexer.FormatErrMsg(&tok.Loc, "Negative size for `alloc` block: %d", alloc_size)
				}
				scope.Allocs[tok.Text] = Allocation{
					Offset: scope.MemSize, Size: alloc_size,
				}
				scope.MemSize += alloc_size

			case lexer.KeywordFunc:
				if err := c.compileFunc(&token, tokens, ctx, scope_name); err != nil {
					return err
				}

			case lexer.KeywordInclude:
				return lexer.FormatErrMsg(&token.Loc, "Include keyword should not appear in here, probably there is a bug in a lexer")
			default:
				return lexer.FormatErrMsg(&token.Loc, "Unhandled KewordType handling: `%s`", token.Text)
			}
		default:
			return lexer.FormatErrMsg(&token.Loc, "Unhandled token: `%s`\n", token.Text)
		}
	}
	return nil
}

func (c *Compiler) CompileTokens(tokens *[]lexer.Token, ctx *Context) error {
	if err := c.compile(tokens, ctx, GlobalScopeName); err != nil {
		return err
	}

	if len(*tokens) != 0 {
		return lexer.FormatErrMsg(nil, "[ERROR] No all tokens were processed")
	}

	if len(c.Blocks.Data) > 0 {
		top := c.Blocks.Data[len(c.Blocks.Data)-1].(*Block)
		return lexer.FormatErrMsg(&top.Tok.Loc, "Unclosed `%s`-block", top.Tok.Text)
	}

	_, exists := ctx.Funcs["main"]
	if !exists {
		return lexer.FormatErrMsg(nil, "No entry point found (function `main` was not defined)")
	}

	return nil
}
