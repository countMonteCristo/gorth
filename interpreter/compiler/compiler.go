package compiler

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"

	"golang.org/x/exp/slices"
)

type Compiler struct {
	Blocks     utils.Stack
	Ops        []vm.Op
	InlinedOps []vm.Op
	Ctx        CompileTimeContext
	debug      bool
}

func NewCompiler(debug bool) *Compiler {
	return &Compiler{
		Blocks: utils.Stack{}, Ops: make([]vm.Op, 0), Ctx: *NewCompileTimeContext(),
		InlinedOps: make([]vm.Op, 0),
		debug:      debug,
	}
}

func (c *Compiler) getCurrentOps() *[]vm.Op {
	if c.Ctx.CurrentFuncIsInlined {
		return &c.InlinedOps
	} else {
		return &c.Ops
	}
}

// Script entry point always must be the last command
func (c *Compiler) EntryPointAddr() types.IntType {
	return c.getCurrentAddr() - 1
}

func (c *Compiler) getCurrentAddr() (addr types.IntType) {
	return types.IntType(len(*c.getCurrentOps()))
}

func (c *Compiler) setOpOperand(addr, value types.IntType) {
	(*c.getCurrentOps())[addr].Operand = value
}

func (c *Compiler) getOpOperand(addr types.IntType) types.IntType {
	return (*c.getCurrentOps())[addr].Operand.(types.IntType)
}

func (c *Compiler) pushOps(scope_name string, ops ...vm.Op) {
	if c.debug {
		for i := range ops {
			ops[i].DebugInfo = scope_name
		}
	}
	*c.getCurrentOps() = append(*c.getCurrentOps(), ops...)
}

func (c *Compiler) prepareInlinedCache(inlined bool) {
	c.Ctx.CurrentFuncIsInlined = inlined
	c.InlinedOps = make([]vm.Op, 0)
}

func (c *Compiler) resetInlinedCache() []vm.Op {
	c.Ctx.CurrentFuncIsInlined = false
	ops := make([]vm.Op, 0)

	// Do not copy OpFuncBegin and OpFuncEnd
	for i := 1; i < len(c.InlinedOps)-1; i++ {
		ops = append(ops, c.InlinedOps[i])
	}

	return ops
}

func (c *Compiler) compileTokenInt(token *lexer.Token, scope_name string) error {
	c.pushOps(scope_name, vm.Op{Typ: vm.OpPushInt, Operand: token.Value.(types.IntType), OpToken: *token})
	return nil
}

func (c *Compiler) compileTokenString(token *lexer.Token, scope_name string) error {
	literal := token.Value.(string)
	literal_addr, exists := c.Ctx.StringsMap[literal]
	if !exists {
		return logger.CompilerError(&token.Loc, "Unknown string literal at compile-time: `%s`", literal)
	}
	c.pushOps(scope_name,
		vm.Op{Typ: vm.OpPushPtr, Operand: literal_addr, OpToken: *token},
		vm.Op{Typ: vm.OpPushInt, Operand: types.IntType(len(literal)), OpToken: *token},
	)
	return nil
}

func (c *Compiler) compileTokenChar(token *lexer.Token, scope_name string) error {
	c.pushOps(scope_name, vm.Op{Typ: vm.OpPushInt, Operand: token.Value.(types.IntType), OpToken: *token})
	return nil
}

func (c *Compiler) compileTokenBool(token *lexer.Token, scope_name string) error {
	c.pushOps(scope_name, vm.Op{Typ: vm.OpPushBool, Operand: token.Value.(types.IntType), OpToken: *token})
	return nil
}

func (c *Compiler) compileTokenIntrinsic(token *lexer.Token, scope_name string, intrinsic lexer.IntrinsicType) error {
	if intrinsic == lexer.IntrinsicOffset || intrinsic == lexer.IntrinsicReset {
		return logger.CompilerError(&token.Loc, "`%s` intrinsic is not allowed outside `const` or `alloc` blocks", token.Text)
	}
	c.pushOps(scope_name, vm.Op{Typ: vm.OpIntrinsic, Operand: intrinsic, OpToken: *token})
	return nil
}

func (c *Compiler) compileLocalConst(token *lexer.Token, val types.IntType, scope_name string) error {
	c.pushOps(scope_name, vm.Op{Typ: vm.OpPushInt, Operand: val, OpToken: *token})
	return nil
}

func (c *Compiler) compileGlobalConst(token *lexer.Token, val types.IntType, scope_name string) error {
	c.pushOps(scope_name, vm.Op{Typ: vm.OpPushInt, Operand: val, OpToken: *token})
	return nil
}

func (c *Compiler) compileLocalAlloc(token *lexer.Token, scope *Scope) error {
	c.pushOps(scope.ScopeName, vm.Op{Typ: vm.OpPushLocalAlloc, Operand: scope.MemSize - scope.Allocs[token.Text].Offset, OpToken: *token})
	return nil
}

func (c *Compiler) compileGlobalAlloc(token *lexer.Token, scope *Scope) error {
	c.pushOps(scope.ScopeName, vm.Op{Typ: vm.OpPushGlobalAlloc, Operand: scope.Allocs[token.Text].Offset, OpToken: *token})
	return nil
}

func (c *Compiler) compileFuncCall(token *lexer.Token, f *Function, scope_name string) error {
	if f.Inlined {
		c.pushOps(scope_name, f.Ops...)
	} else {
		if c.Ctx.CurrentFuncIsInlined {
			return logger.CompilerError(&token.Loc, "Calling non-inlined functions from inlined are not allowed")
		} else {
			c.pushOps(scope_name, vm.Op{Typ: vm.OpCall, Operand: f.Addr - c.getCurrentAddr(), OpToken: *token, Data: f.Sig.Name})
		}
	}

	return nil
}

func (c *Compiler) compileIfBlock(token *lexer.Token, th *lexer.TokenHolder, scope_name string) error {
	c.Blocks.Push(NewBlock(c.getCurrentAddr(), token, lexer.KeywordIf))
	c.pushOps(scope_name, vm.Op{OpToken: *token, Typ: vm.OpJump, Operand: types.IntType(1), Data: vm.NameToOpJumpType[token.Text]})
	return c.compile(th, scope_name)
}

func (c *Compiler) compileElseBlock(token *lexer.Token, th *lexer.TokenHolder, scope_name string) error {

	if c.Blocks.Empty() {
		return logger.CompilerError(&token.Loc, "Unexpected `end` found")
	}
	block := c.Blocks.Pop().(*Block)

	if block.Typ != lexer.KeywordIf {
		return logger.CompilerError(&token.Loc, "`else` may be only inside if-do-end block")
	}
	if block.Tok.Typ != lexer.TokenKeyword {
		return logger.CompilerError(&token.Loc, "Only keywords may form blocks, but not `%s`. Probably bug in lexer", block.Tok.Text)
	}
	block_start_kw := block.Tok.Value.(lexer.KeywordType)

	addr := c.getCurrentAddr()

	switch block_start_kw {
	case lexer.KeywordDo:
		c.setOpOperand(block.Addr, addr-block.Addr+1)
	case lexer.KeywordIf, lexer.KeywordElse, lexer.KeywordEnd, lexer.KeywordWhile:
		return logger.CompilerError(&token.Loc, "`else` may only come after `do` in `if`-block, but got `%s`", block.Tok.Text)
	default:
		return logger.CompilerError(&token.Loc, "Unhandled block start processing")
	}

	c.Blocks.Push(NewBlock(addr, token, block.Typ))
	c.pushOps(scope_name, vm.Op{OpToken: *token, Typ: vm.OpJump, Data: vm.NameToOpJumpType[token.Text]})

	return nil
}

func (c *Compiler) compileWhileBlock(token *lexer.Token, th *lexer.TokenHolder, scope_name string) error {
	c.Blocks.Push(NewBlock(c.getCurrentAddr(), token, lexer.KeywordWhile))
	c.pushOps(scope_name, vm.Op{OpToken: *token, Typ: vm.OpJump, Data: vm.NameToOpJumpType[token.Text], Operand: types.IntType(1)})
	return c.compile(th, scope_name)
}

func (c *Compiler) compileDoBlock(token *lexer.Token, th *lexer.TokenHolder, scope_name string) error {
	if c.Blocks.Empty() {
		return logger.CompilerError(&token.Loc, "Unexpected `do` found")
	}
	block := c.Blocks.Pop().(*Block)
	if block.Tok.Typ != lexer.TokenKeyword {
		return logger.CompilerError(&token.Loc, "Only keywords may form c.Blocks, but got `%s`", block.Tok.Text)
	}

	kw := block.Tok.Value.(lexer.KeywordType)
	if kw != lexer.KeywordWhile && kw != lexer.KeywordIf {
		return logger.CompilerError(&token.Loc, "`do` may come only in `while`, `if` or `func` block, but not `%s`", block.Tok.Text)
	}

	do_addr := c.getCurrentAddr()
	c.Blocks.Push(NewBlock(do_addr, token, block.Typ))
	c.pushOps(scope_name, vm.Op{OpToken: *token, Typ: vm.OpCondJump, Operand: block.Addr - do_addr, Data: token.Text})
	return nil
}

func (c *Compiler) compileJumpKeyword(token *lexer.Token, kw lexer.KeywordType, scope_name string) error {
	if kw != lexer.KeywordContinue && kw != lexer.KeywordBreak {
		return logger.CompilerError(&token.Loc, "Only `break` and `continue` are supported as jumps, but got `%s`", lexer.KeywordName[kw])
	}

	if c.Blocks.Empty() {
		return logger.CompilerError(&token.Loc, "Unexpected `%s` found", token.Text)
	}

	var i int
	for i = len(c.Blocks.Data) - 1; i >= 0; i-- {
		cur_block := c.Blocks.Data[i].(*Block)
		t := &cur_block.Tok

		if t.Typ == lexer.TokenKeyword && t.Value.(lexer.KeywordType) == lexer.KeywordDo && cur_block.Typ == lexer.KeywordWhile {
			cur_block.Jumps = append(cur_block.Jumps, Jump{Keyword: kw, Addr: c.getCurrentAddr()})
			break
		}
	}
	if i < 0 {
		return logger.CompilerError(&token.Loc, "`%s` should be inside while-loop, but it doesn't", token.Text)
	}

	c.pushOps(scope_name, vm.Op{OpToken: *token, Typ: vm.OpJump, Data: vm.NameToOpJumpType[token.Text]})
	return nil
}

func (c *Compiler) compileBreakKeyword(token *lexer.Token, scope_name string) error {
	return c.compileJumpKeyword(token, lexer.KeywordBreak, scope_name)
}

func (c *Compiler) compileContinueKeyword(token *lexer.Token, scope_name string) error {
	return c.compileJumpKeyword(token, lexer.KeywordContinue, scope_name)
}

func (c *Compiler) compileReturnKeyword(token *lexer.Token, scope *Scope) error {
	if scope.ScopeName == GlobalScopeName {
		return logger.CompilerError(&token.Loc, "Could not `return` from global scope, only from function")
	}

	if c.Ctx.CurrentFuncIsInlined {
		return logger.CompilerError(&token.Loc, "`%s` in `inline` function is not allowed", token.Text)
	}

	var i int
	for i = len(c.Blocks.Data) - 1; i >= 0; i-- {
		cur_block := c.Blocks.Data[i].(*Block)
		t := &cur_block.Tok

		if t.Typ == lexer.TokenKeyword && t.Value.(lexer.KeywordType) == lexer.KeywordFunc {
			cur_block.Jumps = append(cur_block.Jumps, Jump{Keyword: lexer.KeywordReturn, Addr: c.getCurrentAddr()})
			break
		}
	}
	if i < 0 {
		return logger.CompilerError(&token.Loc, "`%s` should be inside function, but it doesn't", token.Text)
	}

	c.pushOps(scope.ScopeName, vm.Op{OpToken: *token, Operand: types.IntType(1), Typ: vm.OpJump, Data: vm.NameToOpJumpType[token.Text]})
	return nil
}

func (c *Compiler) compileEndKeyword(token *lexer.Token, scope_name string) error {
	if c.Blocks.Empty() {
		return logger.CompilerError(&token.Loc, "Unexpected `end` found")
	}
	block := c.Blocks.Pop().(*Block)

	if !slices.Contains([]lexer.KeywordType{lexer.KeywordIf, lexer.KeywordWhile, lexer.KeywordFunc}, block.Typ) {
		return logger.CompilerError(&token.Loc, "`end` should close only `if`, `while` or `func` blocks, but not %s", block.Tok.Text)
	}

	if block.Tok.Typ != lexer.TokenKeyword {
		return logger.CompilerError(&token.Loc, "Only keywords may form blocks, but not `%s`. Probably bug in lexer", block.Tok.Text)
	}
	block_start_kw := block.Tok.Value.(lexer.KeywordType)

	op := vm.Op{OpToken: *token, Operand: types.IntType(1), Typ: vm.OpJump, Data: vm.NameToOpJumpType[token.Text]}
	addr := c.getCurrentAddr()
	do_end_diff := addr - block.Addr + 1

	switch block_start_kw {
	case lexer.KeywordDo:
		switch block.Typ {
		case lexer.KeywordWhile: // while-do-end
			do_while_addr_diff := c.getOpOperand(block.Addr)
			for _, jump := range block.Jumps {
				switch jump.Keyword {
				case lexer.KeywordBreak:
					c.setOpOperand(jump.Addr, addr-jump.Addr+1) // break -> end + 1
				case lexer.KeywordContinue:
					c.setOpOperand(jump.Addr, addr-jump.Addr) // continue -> end
				default:
					return logger.CompilerError(&block.Tok.Loc, "Unhandled jump-keyword: `%s`", lexer.KeywordName[jump.Keyword])
				}
			}
			op.Operand = block.Addr + do_while_addr_diff - addr // end -> while
			c.setOpOperand(block.Addr, do_end_diff)             // do -> end + 1 if condition is false
		case lexer.KeywordIf: // if-do-end

			c.setOpOperand(block.Addr, do_end_diff) // do -> end + 1 if condition is false
		default:
			return logger.CompilerError(&token.Loc, "Unhandled block type while compiling `end` keyword")
		}
	case lexer.KeywordElse: // if-do-else-end
		c.setOpOperand(block.Addr, do_end_diff) // do -> end + 1 if condition is false
	case lexer.KeywordFunc:
		for _, jump := range block.Jumps {
			if jump.Keyword == lexer.KeywordReturn {
				c.setOpOperand(jump.Addr, addr-jump.Addr) // return -> end
			} else {
				return logger.CompilerError(&block.Tok.Loc, "Unhandled jump-keyword: `%s` in function", lexer.KeywordName[jump.Keyword])
			}
		}
		op.Typ, op.Data = vm.OpFuncEnd, scope_name

	case lexer.KeywordIf, lexer.KeywordWhile:
		return logger.CompilerError(&block.Tok.Loc, "`%s` block must contain `do` before `end`", lexer.KeywordName[block_start_kw])
	case lexer.KeywordEnd:
		return logger.CompilerError(&token.Loc, "`end` may only close `if-else` or `while-do` blocks, but got `%s`", block.Tok.Text)
	case lexer.KeywordBreak, lexer.KeywordContinue:
		return logger.CompilerError(&block.Tok.Loc, "`%s` keyword shouldn't be in blocks stack", lexer.KeywordName[block_start_kw])

	default:
		return logger.CompilerError(&token.Loc, "Unhandled block start processing (processEndKeyword)")
	}
	c.pushOps(scope_name, op)
	return nil
}

func (c *Compiler) compileNamedBlock(token *lexer.Token, th *lexer.TokenHolder, scope_name string, typ string) (name_token lexer.Token, value types.IntType, err error) {

	if th.Empty() {
		err = logger.CompilerError(&token.Loc, "Expected `%s` name, but got nothing", typ)
		return
	}

	scope := c.Ctx.Scopes[scope_name]

	name_token = *th.GetNextToken()

	if name_token.Typ != lexer.TokenWord {
		err = logger.CompilerError(&token.Loc, "Expected `%s` name to be a word, but got `%s`", typ, name_token.Text)
		return
	}
	defined_token, exists := scope.Names[name_token.Text]
	if exists {
		msg := logger.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
		err = logger.CompilerError(&name_token.Loc, "Redefinition of name `%s` (%s)", name_token.Text, msg)
		return
	}

	defined_token, exists = c.Ctx.GlobalScope().Names[name_token.Text]
	if exists {
		_, func_exists := c.Ctx.Funcs[name_token.Text]
		if func_exists {
			msg := logger.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
			err = logger.CompilerError(&name_token.Loc, "Redefinition of function `%s` (%s)", name_token.Text, msg)
			return
		}

		msg := logger.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
		err = logger.CompilerError(&name_token.Loc, "Redefinition of global name `%s` (%s)", name_token.Text, msg)
		return
	}

	scope.Names[name_token.Text] = name_token

	const_th := lexer.NewTokenHolder()
	for {
		if th.Empty() {
			err = logger.CompilerError(&token.Loc, "Unexpected end while processing `%s` block", typ)
			return
		}

		btok := *th.GetNextToken()
		if btok.Typ == lexer.TokenKeyword && btok.Value.(lexer.KeywordType) == lexer.KeywordEnd {
			break
		}

		const_th.AppendToken(btok)
	}

	value, err = c.constEval(&name_token, const_th, scope)
	return
}

func (c *Compiler) compileFuncSignature(token *lexer.Token, th *lexer.TokenHolder) (inputs, outputs utils.Stack, err error) {
	if th.Empty() {
		err = logger.CompilerError(&token.Loc, "Expected function signature, but got nothing")
		return
	}

	if n := th.NextToken(); n.Typ == lexer.TokenKeyword && !slices.Contains([]lexer.KeywordType{lexer.KeywordDo, lexer.KeywordColon}, n.Value.(lexer.KeywordType)) {
		err = logger.CompilerError(&n.Loc, "Expected function signature, but got `%s` keyword", n.Text)
		return
	}

	curr := &inputs
	for {
		if th.Empty() {
			err = logger.CompilerError(&token.Loc, "Unexpected end of tokens while compiling function signature")
			return
		}

		if n := th.NextToken(); n.Typ == lexer.TokenKeyword && n.Value.(lexer.KeywordType) == lexer.KeywordDo {
			break
		}

		switch next := th.GetNextToken(); next.Typ {
		case lexer.TokenWord:
			datatype, ok := next.Value.(lexer.DataType)
			if !ok {
				err = logger.CompilerError(&next.Loc, "Unexpected word `%s` in function signature", next.Text)
				return
			}

			curr.Push(datatype)
		case lexer.TokenKeyword:
			if next.Value.(lexer.KeywordType) == lexer.KeywordColon {
				curr = &outputs
			} else {
				err = logger.CompilerError(&next.Loc, "Unexpected keyword `%s` in function signature", next.Text)
				return
			}
		default:
			err = logger.CompilerError(&next.Loc, "Unexpected token type `%s` in function signature", next.Text)
			return
		}
	}

	return
}

func (c *Compiler) compileFuncDef(token *lexer.Token, th *lexer.TokenHolder) (name_token lexer.Token, sig FuncSignature, err error) {
	if th.Empty() {
		err = logger.CompilerError(&token.Loc, "Expected `%s` name, but got nothing", token.Text)
		return
	}

	name_token = *th.GetNextToken()
	if name_token.Typ != lexer.TokenWord {
		err = logger.CompilerError(&token.Loc, "Expected `%s` name to be a word, but got `%s`", token.Text, name_token.Text)
		return
	}

	defined_token, exists := c.Ctx.GlobalScope().Names[name_token.Text]
	if exists {
		msg := logger.FormatNoneMsg(&defined_token.Loc, "Previously defined here")
		err = logger.CompilerError(&name_token.Loc, "Redefinition of global name `%s` in function definition (%s)", name_token.Text, msg)
		return
	}

	func_name := name_token.Text
	c.Ctx.GlobalScope().Names[func_name] = name_token

	inputs, outputs, err := c.compileFuncSignature(token, th)
	if err != nil {
		return
	}

	do_token := th.GetNextToken()
	if do_token.Typ != lexer.TokenKeyword {
		err = logger.CompilerError(&do_token.Loc, "Expected `do` to start the function name, but got `%s`", do_token.Text)
		return
	}
	if do_token.Value.(lexer.KeywordType) != lexer.KeywordDo {
		err = logger.CompilerError(&do_token.Loc, "Expected keyword `do` to start the function name, but got `%s`", do_token.Text)
		return
	}
	sig = FuncSignature{Name: func_name, Inputs: inputs, Outputs: outputs}

	return
}

func (c *Compiler) compileFunc(token *lexer.Token, th *lexer.TokenHolder, scope_name string) error {

	if scope_name != GlobalScopeName {
		return logger.CompilerError(&token.Loc, "Cannot define functions inside a function %s", scope_name)
	}
	if _, exists := c.Ctx.Funcs["main"]; exists {
		return logger.CompilerError(&token.Loc, "Cannot define functions after `main`")
	}

	inlined := false
	keyword := token.Value.(lexer.KeywordType)
	if keyword == lexer.KeywordInline {
		next := th.NextToken()
		if next.Typ != lexer.TokenKeyword || next.Value.(lexer.KeywordType) != lexer.KeywordFunc {
			return logger.CompilerError(&next.Loc, "Expected `func` after `inline`, but found `%s`", next.Text)
		} else {
			token = th.GetNextToken()
			inlined = true
		}
	}

	func_token, signature, err := c.compileFuncDef(token, th)
	if err != nil {
		return err
	}

	new_scope := NewScope(signature.Name)
	new_scope.Names[signature.Name] = func_token
	c.Ctx.Scopes[signature.Name] = new_scope

	c.prepareInlinedCache(inlined)

	func_addr := c.getCurrentAddr()
	c.Blocks.Push(NewBlock(func_addr, token, lexer.KeywordFunc))

	c.pushOps(signature.Name, vm.Op{OpToken: *token, Typ: vm.OpFuncBegin, Data: signature.Name})
	if err = c.compile(th, signature.Name); err != nil {
		return err
	}

	// needed only for non-inlined functions
	c.setOpOperand(func_addr, new_scope.MemSize)            // OpFuncBegin $MEM - allocates   $MEM bytes in RAM
	c.setOpOperand(c.getCurrentAddr()-1, new_scope.MemSize) // OpFuncEnd $MEM   - deallocates $MEM bytes in RAM

	c.Ctx.Funcs[signature.Name] = Function{
		Addr: func_addr, Sig: signature, Inlined: inlined,
		Ops: c.resetInlinedCache(),
	}

	return nil
}

func (c *Compiler) constEval(token *lexer.Token, th *lexer.TokenHolder, scope *Scope) (value types.IntType, err error) {
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
					const_stack.Push(vm.SafeArithmeticFunctions[intrinsic](a, b))
				case lexer.IntrinsicDiv:
					b := const_stack.Pop().(types.IntType)
					if b == 0 {
						err = logger.CompilerError(&token.Loc, "Division by zero")
						return
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a / b)
				case lexer.IntrinsicMod:
					b := const_stack.Pop().(types.IntType)
					if b == 0 {
						err = logger.CompilerError(&token.Loc, "Division by zero")
						return
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a % b)
				case lexer.IntrinsicShl:
					b := const_stack.Pop().(types.IntType)
					if b < 0 {
						err = logger.CompilerError(&token.Loc, "Negative shift amount in `<<`: %d", b)
						return
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a << b)
				case lexer.IntrinsicShr:
					b := const_stack.Pop().(types.IntType)
					if b < 0 {
						err = logger.CompilerError(&token.Loc, "Negative shift amount in `>>`: %d", b)
						return
					}
					a := const_stack.Pop().(types.IntType)
					const_stack.Push(a >> b)
				case lexer.IntrinsicOffset:
					off := const_stack.Pop().(types.IntType)
					const_stack.Push(c.Ctx.Offset)
					c.Ctx.Offset += off
				case lexer.IntrinsicReset:
					const_stack.Push(c.Ctx.Offset)
					c.Ctx.Offset = 0
				default:
					err = logger.CompilerError(
						&token.Loc,
						"Unexpected intrinsic in const-block compile-time "+
							"evaluation: `%s`. Supported: [+, -, *, /, %%, >>, <<, offset, reset]", token.Text,
					)
					return
				}
				continue
			}

			val, scope := c.Ctx.GetConst(token.Text, scope.ScopeName)
			if scope != ScopeUnknown {
				const_stack.Push(val)
				continue
			}

			err = logger.CompilerError(&token.Loc, "Unsupported word in compile-time const-block evaluation: `%s`", token.Text)
			return
		default:
			err = logger.CompilerError(&token.Loc, "Unsupported token in compile-time const-block evaluation: `%s`", token.Text)
			return
		}
	}

	if const_stack.Size() > 1 {
		err = logger.CompilerError(&token.Loc, "Unhandled data in compile-time const-block evaluation stack")
		return
	}

	value, _ = const_stack.Pop().(types.IntType)
	return
}

func (c *Compiler) compile(th *lexer.TokenHolder, scope_name string) error {

	scope, exists := c.Ctx.Scopes[scope_name]
	if !exists {
		return logger.CompilerError(nil, "No such scope: `%s`", scope_name)
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
			if err := c.compileTokenString(token, scope_name); err != nil {
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

			val, exists := c.Ctx.GetLocalConst(name, scope_name)
			if exists {
				if err := c.compileLocalConst(token, val, scope_name); err != nil {
					return err
				}
				continue
			}

			_, exists = c.Ctx.GetLocalAlloc(name, scope_name)
			if exists {
				if err := c.compileLocalAlloc(token, scope); err != nil {
					return err
				}
				continue
			}

			val, exists = c.Ctx.GetGlobalConst(name)
			if exists {
				if err := c.compileGlobalConst(token, val, scope_name); err != nil {
					return err
				}
				continue
			}

			_, exists = c.Ctx.GetGlobalAlloc(name)
			if exists {
				if err := c.compileGlobalAlloc(token, scope); err != nil {
					return err
				}
				continue
			}

			function, exists := c.Ctx.Funcs[name]
			if exists {
				if err := c.compileFuncCall(token, &function, scope_name); err != nil {
					return err
				}
				continue
			}

			return logger.CompilerError(&token.Loc, "Unknown word: `%s`", token.Text)

		case lexer.TokenKeyword:
			kw_type := token.Value.(lexer.KeywordType)
			scope := c.Ctx.Scopes[scope_name]

			switch kw_type {
			case lexer.KeywordIf:
				if err := c.compileIfBlock(token, th, scope_name); err != nil {
					return err
				}
			case lexer.KeywordElse:
				if err := c.compileElseBlock(token, th, scope_name); err != nil {
					return err
				}
			case lexer.KeywordEnd:
				return c.compileEndKeyword(token, scope_name) // return from compile() after processing `end`
			case lexer.KeywordWhile:
				if err := c.compileWhileBlock(token, th, scope_name); err != nil {
					return err
				}
			case lexer.KeywordDo:
				if err := c.compileDoBlock(token, th, scope_name); err != nil {
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
			case lexer.KeywordReturn:
				if err := c.compileReturnKeyword(token, scope); err != nil {
					return err
				}

			case lexer.KeywordConst:
				tok, const_value, err := c.compileNamedBlock(token, th, scope_name, token.Text)
				if err != nil {
					return err
				}
				scope.Consts[tok.Text] = const_value
			case lexer.KeywordAlloc:
				if c.Ctx.CurrentFuncIsInlined {
					return logger.CompilerError(&token.Loc, "Local allocations are not allowed in `inline` function")
				}
				tok, alloc_size, err := c.compileNamedBlock(token, th, scope_name, token.Text)
				if err != nil {
					return err
				}
				if alloc_size < 0 {
					return logger.CompilerError(&tok.Loc, "Negative size for `alloc` block: %d", alloc_size)
				}
				scope.Allocs[tok.Text] = Allocation{
					Offset: scope.MemSize, Size: alloc_size,
				}
				scope.MemSize += alloc_size

			case lexer.KeywordFunc, lexer.KeywordInline:
				if err := c.compileFunc(token, th, scope_name); err != nil {
					return err
				}

			case lexer.KeywordInclude:
				return logger.CompilerError(&token.Loc, "Include keyword should not appear in here, probably there is a bug in a lexer")
			default:
				return logger.CompilerError(&token.Loc, "Unhandled keword: `%s`", token.Text)
			}
		default:
			return logger.CompilerError(&token.Loc, "Unhandled token: `%s`\n", token.Text)
		}
	}
	return nil
}

func (c *Compiler) CompileTokens(tokens *lexer.TokenHolder, rts *vm.RuntimeSettings) error {
	// Set memory addresses for string literals
	c.Ctx.PreprocessStringLiterals(tokens, rts.StringLiteralsStart)

	tokens.Reset()
	if err := c.compile(tokens, GlobalScopeName); err != nil {
		return err
	}

	if !tokens.Empty() {
		return logger.CompilerError(nil, "[ERROR] No all tokens were processed")
	}

	if len(c.Blocks.Data) > 0 {
		top := c.Blocks.Data[len(c.Blocks.Data)-1].(*Block)
		return logger.CompilerError(&top.Tok.Loc, "Unclosed `%s`-block", top.Tok.Text)
	}

	f, exists := c.Ctx.Funcs["main"]
	if !exists {
		return logger.CompilerError(nil, "No entry point found (function `main` was not defined)")
	}
	c.pushOps(GlobalScopeName, vm.Op{Typ: vm.OpCall, Operand: f.Addr - c.getCurrentAddr(), Data: f.Sig.Name})

	// Set runtime parameters
	rts.EntryPointAddr = c.EntryPointAddr()
	rts.GlobalMemorySize = c.Ctx.GlobalScope().MemSize
	rts.StringLiterals = &c.Ctx.StringsMap
	rts.OpsCount = c.getCurrentAddr()

	return nil
}
