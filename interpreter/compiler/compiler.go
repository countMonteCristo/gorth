package compiler

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"
	"fmt"

	"golang.org/x/exp/slices"
)

// ---------------------------------------------------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) getCurrentOps() *[]vm.Op {
	if c.Ctx.CurrentFuncIsInlined {
		return &c.InlinedOps
	} else {
		return &c.Ops
	}
}

func (c *Compiler) getCurrentAddr() (addr types.IntType) {
	return types.IntType(len(*c.getCurrentOps()))
}

// ---------------------------------------------------------------------------------------------------------------------

// Script entry point always must be the last command
func (c *Compiler) EntryPointAddr() types.IntType {
	return c.getCurrentAddr() - 1
}

// ---------------------------------------------------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileTokenInt(token *lexer.Token, scope *Scope) error {
	c.pushOps(scope.Name, vm.Op{Typ: vm.OpPushInt, Operand: token.Value.(types.IntType), OpToken: *token})
	return nil
}

func (c *Compiler) compileTokenString(token *lexer.Token, scope *Scope) error {
	literal := token.Value.(string)
	if literal_addr, exists := c.Ctx.StringsMap[literal]; !exists {
		return logger.CompilerError(&token.Loc, "Unknown string literal at compile-time: `%s`", literal)
	} else {
		c.pushOps(scope.Name,
			vm.Op{Typ: vm.OpPushPtr, Operand: literal_addr, OpToken: *token},
			vm.Op{Typ: vm.OpPushInt, Operand: types.IntType(len(literal)), OpToken: *token},
		)
	}
	return nil
}

func (c *Compiler) compileTokenChar(token *lexer.Token, scope *Scope) error {
	c.pushOps(scope.Name, vm.Op{Typ: vm.OpPushInt, Operand: token.Value.(types.IntType), OpToken: *token})
	return nil
}

func (c *Compiler) compileTokenBool(token *lexer.Token, scope *Scope) error {
	c.pushOps(scope.Name, vm.Op{Typ: vm.OpPushBool, Operand: token.Value.(types.IntType), OpToken: *token})
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileTokenIntrinsic(token *lexer.Token, scope *Scope, intrinsic lexer.IntrinsicType) error {
	if intrinsic == lexer.IntrinsicOffset || intrinsic == lexer.IntrinsicReset {
		return logger.CompilerError(&token.Loc, "`%s` intrinsic is not allowed outside `const` or `alloc` blocks", token.Text)
	}
	c.pushOps(scope.Name, vm.Op{Typ: vm.OpIntrinsic, Operand: intrinsic, OpToken: *token})
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileConst(token *lexer.Token, val *Constant, scope *Scope) error {
	if typ, exists := DataTypeToOpType[val.Typ]; !exists {
		return logger.CompilerError(&token.Loc, "Can not compile constant of type `%s`", lexer.DataTypeName[val.Typ])
	} else {
		c.pushOps(scope.Name, vm.Op{Typ: typ, Operand: val.Value, OpToken: *token})
		return nil
	}
}

func (c *Compiler) compileLocalAlloc(token *lexer.Token, scope *Scope) error {
	c.pushOps(scope.Name, vm.Op{Typ: vm.OpPushLocalAlloc, Operand: scope.MemSize - scope.Allocs[token.Text].Offset, OpToken: *token})
	return nil
}

func (c *Compiler) compileGlobalAlloc(token *lexer.Token, scope *Scope) error {
	c.pushOps(scope.Name, vm.Op{Typ: vm.OpPushGlobalAlloc, Operand: scope.Allocs[token.Text].Offset, OpToken: *token})
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileFuncCall(token *lexer.Token, f *Function, scope *Scope) error {
	if f.Inlined {
		c.pushOps(scope.Name, f.Ops...)
	} else {
		if c.Ctx.CurrentFuncIsInlined {
			return logger.CompilerError(&token.Loc, "Calling non-inlined functions from inlined are not allowed")
		} else {
			c.pushOps(scope.Name, vm.Op{Typ: vm.OpCall, Operand: f.Addr - c.getCurrentAddr(), OpToken: *token, Data: f.Sig.Name})
		}
	}
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileIfBlock(token *lexer.Token, th *lexer.TokenHolder, scope *Scope) error {
	c.Blocks.Push(NewBlock(c.getCurrentAddr(), token, lexer.KeywordIf, nil))
	c.pushOps(scope.Name, vm.Op{OpToken: *token, Typ: vm.OpJump, Operand: types.IntType(1), Data: vm.NameToOpJumpType[token.Text]})
	return c.compile(th, scope)
}

func (c *Compiler) compileElseBlock(token *lexer.Token, th *lexer.TokenHolder, scope *Scope) error {

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

	c.Blocks.Push(NewBlock(addr, token, block.Typ, nil))
	c.pushOps(scope.Name, vm.Op{OpToken: *token, Typ: vm.OpJump, Data: vm.NameToOpJumpType[token.Text]})

	return nil
}

func (c *Compiler) compileWhileBlock(token *lexer.Token, th *lexer.TokenHolder, scope *Scope) error {
	c.Blocks.Push(NewBlock(c.getCurrentAddr(), token, lexer.KeywordWhile, nil))
	c.pushOps(scope.Name, vm.Op{OpToken: *token, Typ: vm.OpJump, Data: vm.NameToOpJumpType[token.Text], Operand: types.IntType(1)})
	return c.compile(th, scope)
}

func (c *Compiler) compileDoBlock(token *lexer.Token, th *lexer.TokenHolder, scope *Scope) error {
	if c.Blocks.Empty() {
		return logger.CompilerError(&token.Loc, "Unexpected `do` found")
	}
	block := c.Blocks.Pop().(*Block)
	if block.Tok.Typ != lexer.TokenKeyword {
		return logger.CompilerError(&token.Loc, "Only keywords may form c.Blocks, but got `%s`", block.Tok.Text)
	}

	kw := block.Tok.Value.(lexer.KeywordType)
	if !slices.Contains([]lexer.KeywordType{lexer.KeywordWhile, lexer.KeywordIf, lexer.KeywordCapture}, kw) {
		return logger.CompilerError(&token.Loc, "`do` may come only in `while`, `if`, `func` or `capture` block, but not `%s`", block.Tok.Text)
	}

	do_addr := c.getCurrentAddr()
	c.Blocks.Push(NewBlock(do_addr, token, block.Typ, nil))
	c.pushOps(scope.Name, vm.Op{OpToken: *token, Typ: vm.OpCondJump, Operand: block.Addr - do_addr, Data: token.Text})
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileJumpKeyword(token *lexer.Token, kw lexer.KeywordType, scope *Scope) error {
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

		if cur_block.Typ == lexer.KeywordCapture {
			return logger.CompilerError(&t.Loc, "`%s` inside a capture block is not allowed", token.Text)
		}

		if t.Typ == lexer.TokenKeyword && t.Value.(lexer.KeywordType) == lexer.KeywordDo && cur_block.Typ == lexer.KeywordWhile {
			cur_block.Jumps = append(cur_block.Jumps, Jump{Keyword: kw, Addr: c.getCurrentAddr()})
			break
		}
	}
	if i < 0 {
		return logger.CompilerError(&token.Loc, "`%s` should be inside while-loop, but it doesn't", token.Text)
	}

	c.pushOps(scope.Name, vm.Op{OpToken: *token, Typ: vm.OpJump, Data: vm.NameToOpJumpType[token.Text]})
	return nil
}

func (c *Compiler) compileBreakKeyword(token *lexer.Token, scope *Scope) error {
	return c.compileJumpKeyword(token, lexer.KeywordBreak, scope)
}

func (c *Compiler) compileContinueKeyword(token *lexer.Token, scope *Scope) error {
	return c.compileJumpKeyword(token, lexer.KeywordContinue, scope)
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileReturnKeyword(token *lexer.Token, scope *Scope) error {
	if scope.Name == GlobalScopeName {
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

	c.pushOps(scope.Name, vm.Op{OpToken: *token, Operand: types.IntType(1), Typ: vm.OpJump, Data: vm.NameToOpJumpType[token.Text]})
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileEndKeyword(token *lexer.Token, scope *Scope) error {
	if c.Blocks.Empty() {
		return logger.CompilerError(&token.Loc, "Unexpected `end` found")
	}
	block := c.Blocks.Pop().(*Block)

	if !slices.Contains([]lexer.KeywordType{lexer.KeywordIf, lexer.KeywordWhile, lexer.KeywordFunc, lexer.KeywordCapture}, block.Typ) {
		return logger.CompilerError(&token.Loc, "`end` should close only `if`, `while`, `func` or `capture` blocks, but not %s", block.Tok.Text)
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
		op.Typ, op.Data = vm.OpFuncEnd, scope.Name

	case lexer.KeywordIf, lexer.KeywordWhile:
		return logger.CompilerError(&block.Tok.Loc, "`%s` block must contain `do` before `end`", lexer.KeywordName[block_start_kw])
	case lexer.KeywordEnd:
		return logger.CompilerError(&token.Loc, "`end` may only close `if-else` or `while-do` blocks, but got `%s`", block.Tok.Text)
	case lexer.KeywordBreak, lexer.KeywordContinue:
		return logger.CompilerError(&block.Tok.Loc, "`%s` keyword shouldn't be in blocks stack", lexer.KeywordName[block_start_kw])

	case lexer.KeywordCapture:
		count_drops := block.Data.(types.IntType)
		for i := 0; i < int(count_drops); i++ {
			v := scope.Captures.Pop().(CapturedVal)
			delete(scope.Names, v.Name)
		}
		c.pushOps(scope.Name, vm.Op{Typ: vm.OpDropCaptures, OpToken: *token, Operand: block.Data.(types.IntType)})

		return nil
		// return logger.CompilerError(&block.Tok.Loc, "`end` for captures is not implemented yet")

	default:
		return logger.CompilerError(&token.Loc, "Unhandled block start processing (processEndKeyword)")
	}
	c.pushOps(scope.Name, op)
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileNamedBlock(token *lexer.Token, th *lexer.TokenHolder, scope *Scope) (name_token lexer.Token, value Constant, err error) {
	if th.Empty() {
		err = logger.CompilerError(&token.Loc, "Expected `%s` name, but got nothing", token.Text)
		return
	}

	name_token = *th.GetNextToken()

	if name_token.Typ != lexer.TokenWord {
		err = logger.CompilerError(&token.Loc, "Expected `%s` name to be a word, but got `%s`", token.Text, name_token.Text)
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

	value, err = c.constEval(&name_token, th, scope)
	return
}

func (c *Compiler) checkConstTypes(token *lexer.Token, expected, actual lexer.DataTypes) error {
	if len(expected) != len(actual) {
		logger.CompilerCrash(&token.Loc, "Sizes of expected and actual datatypes does not match")
	}

	for i := range expected {
		if actual[i] != expected[i] {
			return logger.CompilerError(&token.Loc,
				"Unsupported types for `%s` in named block: expected %s, got %s",
				token.Text, expected, actual,
			)
		}
	}
	return nil
}

func (c *Compiler) checkConstStackSize(token *lexer.Token, stack *utils.Stack, count int) error {
	if stack.Size() < count {
		return logger.CompilerError(
			&token.Loc, "Const stack size has %d elements but `%s` needs to have at least %d",
			stack.Size(), token.Text, count,
		)
	}
	return nil
}

func (c *Compiler) constEval(token *lexer.Token, th *lexer.TokenHolder, scope *Scope) (value Constant, err error) {
	const_stack := &utils.Stack{}

loop:
	for !th.Empty() {
		switch tok := th.GetNextToken(); tok.Typ {
		case lexer.TokenInt:
			const_stack.Push(Constant{Value: tok.Value.(types.IntType), Typ: lexer.DataTypeInt})
		case lexer.TokenBool:
			const_stack.Push(Constant{Value: tok.Value.(types.IntType), Typ: lexer.DataTypeBool})
		case lexer.TokenWord:
			if intrinsic, exists := lexer.WordToIntrinsic[tok.Text]; exists {
				switch intrinsic {
				case lexer.IntrinsicPlus, lexer.IntrinsicMinus, lexer.IntrinsicMul, lexer.IntrinsicBitAnd, lexer.IntrinsicBitXor, lexer.IntrinsicBitOr:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop().(Constant)
					a := const_stack.Pop().(Constant)
					if err = c.checkConstTypes(tok, lexer.DataTypes{lexer.DataTypeInt, lexer.DataTypeInt}, lexer.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: vm.SafeArithmeticFunctions[intrinsic](a.Value, b.Value), Typ: lexer.DataTypeInt})
				case lexer.IntrinsicBitNot:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					a := const_stack.Pop().(Constant)
					if err = c.checkConstTypes(tok, lexer.DataTypes{lexer.DataTypeInt}, lexer.DataTypes{a.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: ^a.Value, Typ: lexer.DataTypeInt})
				case lexer.IntrinsicDiv:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop().(Constant)
					if b.Value == 0 {
						err = logger.CompilerError(&tok.Loc, "Division by zero")
						return
					}
					a := const_stack.Pop().(Constant)
					if err = c.checkConstTypes(tok, lexer.DataTypes{lexer.DataTypeInt, lexer.DataTypeInt}, lexer.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: a.Value / b.Value, Typ: lexer.DataTypeInt})
				case lexer.IntrinsicMod:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop().(Constant)
					if b.Value == 0 {
						err = logger.CompilerError(&tok.Loc, "Modulo by zero")
						return
					}
					a := const_stack.Pop().(Constant)
					if err = c.checkConstTypes(tok, lexer.DataTypes{lexer.DataTypeInt, lexer.DataTypeInt}, lexer.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: a.Value % b.Value, Typ: lexer.DataTypeInt})
				case lexer.IntrinsicShl:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop().(Constant)
					if b.Value < 0 {
						err = logger.CompilerError(&tok.Loc, "Negative shift amount in `<<`: %d", b)
						return
					}
					a := const_stack.Pop().(Constant)
					if err = c.checkConstTypes(tok, lexer.DataTypes{lexer.DataTypeInt, lexer.DataTypeInt}, lexer.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: a.Value << b.Value, Typ: lexer.DataTypeInt})
				case lexer.IntrinsicShr:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop().(Constant)
					if b.Value < 0 {
						err = logger.CompilerError(&tok.Loc, "Negative shift amount in `>>`: %d", b)
						return
					}
					a := const_stack.Pop().(Constant)
					if err = c.checkConstTypes(tok, lexer.DataTypes{lexer.DataTypeInt, lexer.DataTypeInt}, lexer.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: a.Value >> b.Value, Typ: lexer.DataTypeInt})
				case lexer.IntrinsicOffset:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					off := const_stack.Pop().(Constant)
					if err = c.checkConstTypes(tok, lexer.DataTypes{lexer.DataTypeInt}, lexer.DataTypes{off.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: c.Ctx.Offset, Typ: lexer.DataTypeInt})
					c.Ctx.Offset += off.Value
				case lexer.IntrinsicGe, lexer.IntrinsicGt, lexer.IntrinsicLe, lexer.IntrinsicLt, lexer.IntrinsicEq, lexer.IntrinsicNe:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop().(Constant)
					a := const_stack.Pop().(Constant)
					if err = c.checkConstTypes(tok, lexer.DataTypes{lexer.DataTypeInt, lexer.DataTypeInt}, lexer.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: vm.ComparableFunctions[intrinsic](a.Value, b.Value), Typ: lexer.DataTypeBool})
				case lexer.IntrinsicLogicalAnd, lexer.IntrinsicLogicalOr:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop().(Constant)
					a := const_stack.Pop().(Constant)
					if err = c.checkConstTypes(tok, lexer.DataTypes{lexer.DataTypeBool, lexer.DataTypeBool}, lexer.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: vm.LogicalFunctions[intrinsic](a.Value, b.Value), Typ: lexer.DataTypeBool})
				case lexer.IntrinsicLogicalNot:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					a := const_stack.Pop().(Constant)
					if err = c.checkConstTypes(tok, lexer.DataTypes{lexer.DataTypeBool}, lexer.DataTypes{a.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: vm.B2I(!vm.I2B(a.Value)), Typ: lexer.DataTypeBool})
				case lexer.IntrinsicReset:
					const_stack.Push(Constant{Value: c.Ctx.Offset, Typ: lexer.DataTypeInt})
					c.Ctx.Offset = 0
				case lexer.IntrinsicCastInt:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					a := const_stack.Pop().(Constant)
					const_stack.Push(Constant{Value: a.Value, Typ: lexer.DataTypeInt})
				case lexer.IntrinsicCastBool:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					a := const_stack.Pop().(Constant)
					const_stack.Push(Constant{Value: a.Value, Typ: lexer.DataTypeBool})
				case lexer.IntrinsicCastPtr:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					a := const_stack.Pop().(Constant)
					const_stack.Push(Constant{Value: a.Value, Typ: lexer.DataTypePtr})
				case lexer.IntrinsicDebug:
					values := make([]types.IntType, const_stack.Size())
					for i := range const_stack.Data {
						values[i] = const_stack.Data[i].(Constant).Value
					}
					fmt.Println(logger.FormatNoneMsg(&tok.Loc, "Const stack values: %v", values))
				case lexer.IntrinsicTypeDebug:
					values := make(lexer.DataTypes, const_stack.Size())
					for i := range const_stack.Data {
						values[i] = const_stack.Data[i].(Constant).Typ
					}
					fmt.Println(logger.FormatNoneMsg(&tok.Loc, "Const stack types: %s", values))
				default:
					err = logger.CompilerError(
						&tok.Loc,
						"Unexpected intrinsic in named-block compile-time evaluation: `%s`. Supported: "+
							"[+, -, *, /, %%, &, ^, |, ~, >>, <<, offset, reset, "+
							"&&, ||, !, cast(int), cast(bool), cast(ptr)]",
						tok.Text,
					)
					return
				}
				continue
			}

			if val, const_scope := c.Ctx.GetConst(tok.Text, scope.Name); const_scope != ScopeUnknown {
				const_stack.Push(*val)
				continue
			}

			err = logger.CompilerError(&tok.Loc, "Unsupported word in compile-time const-block evaluation: `%s`", tok.Text)
			return
		case lexer.TokenKeyword:
			switch tok.Value.(lexer.KeywordType) {
			case lexer.KeywordEnd:
				break loop
			default:
				err = logger.CompilerError(&tok.Loc, "Unsupported keyword in compile-time const-block evaluation: `%s`", tok.Text)
				return
			}
		default:
			err = logger.CompilerError(&tok.Loc, "Unsupported token in compile-time const-block evaluation: `%s`", tok.Text)
			return
		}
	}

	if const_stack.Size() > 1 {
		err = logger.CompilerError(&token.Loc, "Unhandled data in compile-time const-block evaluation stack")
		return
	}

	value, _ = const_stack.Pop().(Constant)
	return
}

// ---------------------------------------------------------------------------------------------------------------------

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

func (c *Compiler) compileFunc(token *lexer.Token, th *lexer.TokenHolder, scope *Scope) error {

	if scope.Name != GlobalScopeName {
		return logger.CompilerError(&token.Loc, "Cannot define functions inside a function %s", scope.Name)
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
	c.Ctx.GlobalScope().Names[signature.Name] = func_token

	c.prepareInlinedCache(inlined)

	func_addr := c.getCurrentAddr()
	c.Blocks.Push(NewBlock(func_addr, token, lexer.KeywordFunc, nil))

	c.pushOps(signature.Name, vm.Op{OpToken: *token, Typ: vm.OpFuncBegin, Data: signature.Name})
	if err = c.compile(th, new_scope); err != nil {
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

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) parseCapturedVal(th *lexer.TokenHolder, scope *Scope) (*CapturedVal, error) {
	if th.Empty() {
		return nil, logger.CompilerError(nil, "Expected captured value name but got nothing")
	}

	name_tok := th.GetNextToken()
	if name_tok.Typ != lexer.TokenWord {
		return nil, logger.CompilerError(&name_tok.Loc, "Expected captured value name but got `%s`", name_tok.Text)
	}
	name := name_tok.Text
	if defined, exists := scope.Names[name]; exists {
		msg := logger.FormatNoneMsg(&defined.Loc, "previously defined here")
		return nil, logger.CompilerError(&name_tok.Loc, "Can not capture name `%s` (%s)", name, msg)
	}
	if defined, exists := c.Ctx.GlobalScope().Names[name]; exists {
		msg := logger.FormatNoneMsg(&defined.Loc, "previously defined in global scope here")
		return nil, logger.CompilerError(&name_tok.Loc, "Can not capture name `%s` (%s)", name, msg)
	}

	if th.Empty() {
		return nil, logger.CompilerError(nil, "Expected captured value type but got nothing")
	}

	typ_tok := th.GetNextToken()
	if typ_tok.Typ != lexer.TokenWord {
		return nil, logger.CompilerError(&typ_tok.Loc, "Expected captured value type but got `%s`", typ_tok.Text)
	}
	typ, ok := lexer.WordToDataType[typ_tok.Text]
	if !ok {
		return nil, logger.CompilerError(&typ_tok.Loc, "Unknown type `%s`", typ_tok.Text)
	}
	if typ == lexer.DataTypeAny {
		return nil, logger.CompilerError(&typ_tok.Loc, "`any` type is not allowed in capture list")
	}

	return &CapturedVal{Name: name, Typ: typ, Token: name_tok}, nil
}

func (c *Compiler) compileCaptureList(token *lexer.Token, th *lexer.TokenHolder, scope *Scope) (*CaptureList, error) {
	captures := NewCaptureList()

	for !th.Empty() {
		t := th.NextToken()
		if t.Typ == lexer.TokenKeyword && t.Value.(lexer.KeywordType) == lexer.KeywordDo {
			th.GetNextToken()
			break
		}

		val, err := c.parseCapturedVal(th, scope)
		if err != nil {
			return nil, err
		}
		captures.Append(*val)
	}
	return captures, nil
}

func (c *Compiler) compileCaptureKeyword(token *lexer.Token, th *lexer.TokenHolder, scope *Scope) error {
	if scope.Name == GlobalScopeName {
		return logger.CompilerError(&token.Loc, "Cannot capture variables inside global scope")
	}

	captures, err := c.compileCaptureList(token, th, scope)
	if err != nil {
		return err
	}

	val_types := lexer.DataTypes{}
	for _, v := range captures.Vals {
		c.Ctx.Scopes[scope.Name].Captures.Push(v)
		c.Ctx.Scopes[scope.Name].Names[v.Name] = *v.Token
		val_types = append(val_types, v.Typ)
	}

	cap_count := types.IntType(len(captures.Vals))
	c.pushOps(scope.Name, vm.Op{Typ: vm.OpCapture, Operand: cap_count, OpToken: *token, Data: val_types})

	c.Blocks.Push(NewBlock(-1, token, lexer.KeywordCapture, cap_count))
	if err := c.compile(th, scope); err != nil {
		return err
	}
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compile(th *lexer.TokenHolder, scope *Scope) error {
	var token *lexer.Token
	for !th.Empty() {
		token = th.GetNextToken()

		switch token.Typ {
		case lexer.TokenInt:
			if err := c.compileTokenInt(token, scope); err != nil {
				return err
			}
		case lexer.TokenString:
			if err := c.compileTokenString(token, scope); err != nil {
				return err
			}
		case lexer.TokenChar:
			if err := c.compileTokenChar(token, scope); err != nil {
				return err
			}
		case lexer.TokenBool:
			if err := c.compileTokenBool(token, scope); err != nil {
				return err
			}
		case lexer.TokenWord:
			name := token.Text

			if intrinsic, exists := lexer.WordToIntrinsic[name]; exists {
				if err := c.compileTokenIntrinsic(token, scope, intrinsic); err != nil {
					return err
				}
				continue
			}

			if val, exists := c.Ctx.GetLocalConst(name, scope.Name); exists {
				if err := c.compileConst(token, val, scope); err != nil {
					return err
				}
				continue
			}

			if _, exists := c.Ctx.GetLocalAlloc(name, scope.Name); exists {
				if err := c.compileLocalAlloc(token, scope); err != nil {
					return err
				}
				continue
			}

			if val, exists := c.Ctx.GetGlobalConst(name); exists {
				if err := c.compileConst(token, val, scope); err != nil {
					return err
				}
				continue
			}

			if _, exists := c.Ctx.GetGlobalAlloc(name); exists {
				if err := c.compileGlobalAlloc(token, scope); err != nil {
					return err
				}
				continue
			}

			if function, exists := c.Ctx.Funcs[name]; exists {
				if err := c.compileFuncCall(token, &function, scope); err != nil {
					return err
				}
				continue
			}

			if index, exists := scope.GetCapturedValue(name); exists {
				c.pushOps(scope.Name, vm.Op{OpToken: *token, Typ: vm.OpPushCaptured, Operand: index, Data: scope.Captures.Data[scope.Captures.Size()-1-int(index)].(CapturedVal).Typ})
				continue
			}

			return logger.CompilerError(&token.Loc, "Unknown word: `%s`", token.Text)
		case lexer.TokenKeyword:
			switch kw_type := token.Value.(lexer.KeywordType); kw_type {
			case lexer.KeywordIf:
				if err := c.compileIfBlock(token, th, scope); err != nil {
					return err
				}
			case lexer.KeywordElse:
				if err := c.compileElseBlock(token, th, scope); err != nil {
					return err
				}
			case lexer.KeywordEnd:
				return c.compileEndKeyword(token, scope) // return from compile() after processing `end`

			case lexer.KeywordWhile:
				if err := c.compileWhileBlock(token, th, scope); err != nil {
					return err
				}
			case lexer.KeywordDo:
				if err := c.compileDoBlock(token, th, scope); err != nil {
					return err
				}
			case lexer.KeywordBreak:
				if err := c.compileBreakKeyword(token, scope); err != nil {
					return err
				}
			case lexer.KeywordContinue:
				if err := c.compileContinueKeyword(token, scope); err != nil {
					return err
				}
			case lexer.KeywordReturn:
				if err := c.compileReturnKeyword(token, scope); err != nil {
					return err
				}

			case lexer.KeywordConst:
				tok, const_value, err := c.compileNamedBlock(token, th, scope)
				if err != nil {
					return err
				}
				scope.Consts[tok.Text] = const_value
			case lexer.KeywordAlloc:
				if c.Ctx.CurrentFuncIsInlined {
					return logger.CompilerError(&token.Loc, "Local allocations are not allowed in `inline` function")
				}
				tok, alloc_size, err := c.compileNamedBlock(token, th, scope)
				if err != nil {
					return err
				}
				if alloc_size.Value < 0 {
					return logger.CompilerError(&tok.Loc, "Negative size for `alloc` block: %d", alloc_size)
				}
				if alloc_size.Typ != lexer.DataTypeInt {
					return logger.CompilerError(&tok.Loc, "Only `int` type is supported for `alloc` value, got %s", lexer.DataTypeName[alloc_size.Typ])
				}
				scope.Allocs[tok.Text] = Allocation{
					Offset: scope.MemSize, Size: alloc_size.Value,
				}
				scope.MemSize += alloc_size.Value

			case lexer.KeywordFunc, lexer.KeywordInline:
				if err := c.compileFunc(token, th, scope); err != nil {
					return err
				}

			case lexer.KeywordInclude:
				return logger.CompilerError(&token.Loc, "`include` keyword should not appear in here, probably there is a bug in a lexer")
			case lexer.KeywordCapture:
				if err := c.compileCaptureKeyword(token, th, scope); err != nil {
					return err
				}
			default:
				return logger.CompilerError(&token.Loc, "Unhandled keword: `%s`", token.Text)
			}
		default:
			return logger.CompilerError(&token.Loc, "Unhandled token: `%s`\n", token.Text)
		}
	}
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) CompileTokens(tokens *lexer.TokenHolder, rts *vm.RuntimeSettings) error {
	// Set memory addresses for string literals
	c.Ctx.PreprocessStringLiterals(tokens, rts.StringLiteralsStart)

	tokens.Reset()
	if err := c.compile(tokens, c.Ctx.GlobalScope()); err != nil {
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

// ---------------------------------------------------------------------------------------------------------------------
