package compiler

import (
	"Gorth/interpreter/datatypes"
	"Gorth/interpreter/intrinsics"
	"Gorth/interpreter/keywords"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/operations"
	"Gorth/interpreter/settings"
	"Gorth/interpreter/tokens"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"Gorth/interpreter/vm"
	"fmt"

	"golang.org/x/exp/slices"
)

// ---------------------------------------------------------------------------------------------------------------------

type Compiler struct {
	Blocks     BlockStack
	Ops        []operations.Op
	InlinedOps []operations.Op
	Ctx        CompileTimeContext
	debug      bool
}

func NewCompiler(debug bool) *Compiler {
	return &Compiler{
		Blocks: BlockStack{}, Ops: make([]operations.Op, 0), Ctx: *NewCompileTimeContext(),
		InlinedOps: make([]operations.Op, 0),
		debug:      debug,
	}
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) getCurrentOps() *[]operations.Op {
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

func (c *Compiler) pushOps(scope_name string, ops ...operations.Op) {
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
	c.InlinedOps = make([]operations.Op, 0)
}

func (c *Compiler) resetInlinedCache() []operations.Op {
	c.Ctx.CurrentFuncIsInlined = false
	ops := make([]operations.Op, len(c.InlinedOps))
	copy(ops, c.InlinedOps)

	return ops
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileTokenInt(token *tokens.Token, scope *Scope) error {
	c.pushOps(scope.Name, operations.Op{Typ: operations.OpPushInt, Operand: token.Value.(types.IntType), Token: token})
	return nil
}

func (c *Compiler) compileTokenString(token *tokens.Token, scope *Scope) error {
	literal := token.Value.(string)
	if literal_addr, exists := c.Ctx.StringsMap[literal]; !exists {
		return logger.CompilerError(&token.Loc, "Unknown string literal at compile-time: `%s`", literal)
	} else {
		c.pushOps(scope.Name,
			operations.Op{Typ: operations.OpPushPtr, Operand: literal_addr, Token: token},
			operations.Op{Typ: operations.OpPushInt, Operand: types.IntType(len(literal)), Token: token},
		)
	}
	return nil
}

func (c *Compiler) compileTokenChar(token *tokens.Token, scope *Scope) error {
	c.pushOps(scope.Name, operations.Op{Typ: operations.OpPushInt, Operand: token.Value.(types.IntType), Token: token})
	return nil
}

func (c *Compiler) compileTokenBool(token *tokens.Token, scope *Scope) error {
	c.pushOps(scope.Name, operations.Op{Typ: operations.OpPushBool, Operand: token.Value.(types.IntType), Token: token})
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileTokenIntrinsic(token *tokens.Token, scope *Scope, intrinsic intrinsics.IntrinsicType) error {
	if intrinsic == intrinsics.IntrinsicOffset || intrinsic == intrinsics.IntrinsicReset {
		return logger.CompilerError(&token.Loc, "`%s` intrinsic is not allowed outside `const` or `alloc` blocks", token.Text)
	}
	c.pushOps(scope.Name, operations.Op{Typ: operations.OpIntrinsic, Operand: intrinsic, Token: token})
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileConst(token *tokens.Token, val *Constant, scope *Scope) error {
	if typ, exists := DataType2OpType[val.Typ]; !exists {
		return logger.CompilerError(&token.Loc, "Can not compile constant of type `%s`", datatypes.DataType2Str[val.Typ])
	} else {
		c.pushOps(scope.Name, operations.Op{Typ: typ, Operand: val.Value, Token: token})
		return nil
	}
}

func (c *Compiler) compileLocalAlloc(token *tokens.Token, scope *Scope) error {
	c.pushOps(scope.Name, operations.Op{Typ: operations.OpPushLocalAlloc, Operand: scope.Allocs[token.Text].Offset, Token: token})
	return nil
}

func (c *Compiler) compileGlobalAlloc(token *tokens.Token, scope *Scope) error {
	c.pushOps(scope.Name, operations.Op{Typ: operations.OpPushGlobalAlloc, Operand: c.Ctx.GlobalScope().Allocs[token.Text].Offset, Token: token})
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileFuncCall(token *tokens.Token, f *Function, scope *Scope) error {
	if f.Inlined {
		if scope.Name == f.Sig.Name {
			return logger.CompilerError(&token.Loc, "Recursion is not allowed inside inline functions")
		}
		c.pushOps(scope.Name, f.Ops[1:(len(f.Ops)-1)]...) // do not copy OpFuncBegin and OpFuncEnd for inline function
	} else {
		if c.Ctx.CurrentFuncIsInlined {
			return logger.CompilerError(&token.Loc, "Calling non-inlined functions from inlined are not allowed")
		} else {
			c.pushOps(scope.Name, operations.Op{Typ: operations.OpCall, Operand: f.Addr - c.getCurrentAddr(), Token: token, Data: f.Sig.Name})
		}
	}
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileIfBlock(token *tokens.Token, th *tokens.TokenHolder, scope *Scope) error {
	c.Blocks.Push(NewBlock(c.getCurrentAddr(), token, keywords.KeywordIf, nil, nil))
	c.pushOps(scope.Name, operations.Op{Token: token, Typ: operations.OpJump, Operand: types.IntType(1), Data: operations.Str2OpJumpType[token.Text]})
	return c.compile(th, scope)
}

func (c *Compiler) compileElifBlock(token *tokens.Token, _ *tokens.TokenHolder, scope *Scope) error {
	if c.Blocks.Empty() {
		return logger.CompilerError(&token.Loc, "Unexpected `elif` found")
	}
	block := c.Blocks.Pop()

	if block.Typ != keywords.KeywordDo && block.Parent.Typ != keywords.KeywordIf {
		return logger.CompilerError(&token.Loc, "`elif` may come only after `if` or `elif`")
	}
	addr := c.getCurrentAddr()

	// save elif address for setting its operand to end
	block.Parent.Jumps = append(block.Parent.Jumps, Jump{Keyword: keywords.KeywordElif, Addr: addr})

	c.setOpOperand(block.Addr, addr-block.Addr+1) // do -> [elif]+1

	c.Blocks.Push(NewBlock(addr, token, keywords.KeywordElif, nil, block.Parent))
	c.pushOps(scope.Name, operations.Op{Token: token, Typ: operations.OpJump, Data: operations.Str2OpJumpType[token.Text]})

	return nil
}

func (c *Compiler) compileElseBlock(token *tokens.Token, _ *tokens.TokenHolder, scope *Scope) error {
	if c.Blocks.Empty() {
		return logger.CompilerError(&token.Loc, "Unexpected `else` found")
	}
	block := c.Blocks.Pop()

	if block.Typ != keywords.KeywordDo && block.Parent.Typ != keywords.KeywordIf {
		return logger.CompilerError(&token.Loc, "`else` may come only after `if` or `elif`")
	}

	addr := c.getCurrentAddr()

	// save `else` address for setting its operand to end
	block.Parent.Jumps = append(block.Parent.Jumps, Jump{Keyword: keywords.KeywordElse, Addr: addr})

	c.setOpOperand(block.Addr, addr-block.Addr+1) // do -> [else] + 1

	c.Blocks.Push(NewBlock(addr, token, keywords.KeywordElse, nil, block.Parent))
	c.pushOps(scope.Name, operations.Op{Token: token, Typ: operations.OpJump, Data: operations.Str2OpJumpType[token.Text]})

	return nil
}

func (c *Compiler) compileWhileBlock(token *tokens.Token, th *tokens.TokenHolder, scope *Scope) error {
	c.Blocks.Push(NewBlock(c.getCurrentAddr(), token, keywords.KeywordWhile, nil, nil))
	c.pushOps(scope.Name, operations.Op{Token: token, Typ: operations.OpJump, Data: operations.Str2OpJumpType[token.Text], Operand: types.IntType(1)})
	return c.compile(th, scope)
}

func (c *Compiler) compileDoBlock(token *tokens.Token, _ *tokens.TokenHolder, scope *Scope) error {
	if c.Blocks.Empty() {
		return logger.CompilerError(&token.Loc, "Unexpected `do` found")
	}
	parent := c.Blocks.Top()
	if !slices.Contains([]keywords.KeywordType{keywords.KeywordWhile, keywords.KeywordIf, keywords.KeywordElif, keywords.KeywordCapture}, parent.Typ) {
		return logger.CompilerError(&token.Loc, "`do` may come only in `while`, `if`, `elif` or `capture` block, but not `%s`", keywords.Keyword2Str[parent.Typ])
	}

	if parent.Typ == keywords.KeywordElif {
		parent = c.Blocks.Pop().Parent
	}

	do_addr := c.getCurrentAddr()
	c.Blocks.Push(NewBlock(do_addr, token, keywords.KeywordDo, nil, parent))
	c.pushOps(scope.Name, operations.Op{Token: token, Typ: operations.OpCondJump, Operand: parent.Addr - do_addr, Data: token.Text})
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileJumpKeyword(token *tokens.Token, kw keywords.KeywordType, scope *Scope) error {
	if kw != keywords.KeywordContinue && kw != keywords.KeywordBreak {
		return logger.CompilerError(&token.Loc, "Only `break` and `continue` are supported as jumps, but got `%s`", keywords.Keyword2Str[kw])
	}

	if c.Blocks.Empty() {
		return logger.CompilerError(&token.Loc, "Unexpected `%s` found", token.Text)
	}

	var i int
	for i = len(c.Blocks.Data) - 1; i >= 0; i-- {
		cur_block := c.Blocks.Data[i]

		if cur_block.Typ == keywords.KeywordCapture {
			return logger.CompilerError(&cur_block.Tok.Loc, "`%s` inside a capture block is not allowed", token.Text)
		}

		if cur_block.Parent != nil && cur_block.Typ == keywords.KeywordDo && cur_block.Parent.Typ == keywords.KeywordWhile {
			cur_block.Jumps = append(cur_block.Jumps, Jump{Keyword: kw, Addr: c.getCurrentAddr()})
			break
		}
	}
	if i < 0 {
		return logger.CompilerError(&token.Loc, "`%s` should be inside while-loop, but it doesn't", token.Text)
	}

	c.pushOps(scope.Name, operations.Op{Token: token, Typ: operations.OpJump, Data: operations.Str2OpJumpType[token.Text]})
	return nil
}

func (c *Compiler) compileBreakKeyword(token *tokens.Token, scope *Scope) error {
	return c.compileJumpKeyword(token, keywords.KeywordBreak, scope)
}

func (c *Compiler) compileContinueKeyword(token *tokens.Token, scope *Scope) error {
	return c.compileJumpKeyword(token, keywords.KeywordContinue, scope)
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileReturnKeyword(token *tokens.Token, scope *Scope) error {
	if scope.Name == GlobalScopeName {
		return logger.CompilerError(&token.Loc, "Could not `return` from global scope, only from function")
	}

	if c.Ctx.CurrentFuncIsInlined {
		return logger.CompilerError(&token.Loc, "`%s` in `inline` function is not allowed", token.Text)
	}

	var i int
	for i = len(c.Blocks.Data) - 1; i >= 0; i-- {
		cur_block := c.Blocks.Data[i]
		t := cur_block.Tok

		if t.Typ == tokens.TokenKeyword && t.Value.(keywords.KeywordType) == keywords.KeywordFunc {
			cur_block.Jumps = append(cur_block.Jumps, Jump{Keyword: keywords.KeywordReturn, Addr: c.getCurrentAddr()})
			break
		}
	}
	if i < 0 {
		return logger.CompilerError(&token.Loc, "`%s` should be inside function, but it doesn't", token.Text)
	}

	c.pushOps(scope.Name, operations.Op{Token: token, Operand: types.IntType(1), Typ: operations.OpJump, Data: operations.Str2OpJumpType[token.Text]})
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileEndKeyword(token *tokens.Token, scope *Scope) error {
	if c.Blocks.Empty() {
		return logger.CompilerError(&token.Loc, "Unexpected `end` found")
	}
	block := c.Blocks.Pop()

	if !slices.Contains([]keywords.KeywordType{keywords.KeywordDo, keywords.KeywordElse, keywords.KeywordFunc, keywords.KeywordCapture}, block.Typ) {
		return logger.CompilerError(&token.Loc, "`end` should close only `do`, `else`, `func` or `capture` blocks, but not `%s`", keywords.Keyword2Str[block.Typ])
	}

	op := operations.Op{Token: token, Operand: types.IntType(1), Typ: operations.OpJump, Data: operations.Str2OpJumpType[token.Text]}
	addr := c.getCurrentAddr()
	end_diff := addr - block.Addr + 1

	switch block.Typ {
	case keywords.KeywordDo:
		switch block.Parent.Typ {
		case keywords.KeywordWhile: // while-do-end
			for _, jump := range block.Jumps {
				switch jump.Keyword {
				case keywords.KeywordBreak:
					c.setOpOperand(jump.Addr, addr-jump.Addr+1) // break -> end + 1
				case keywords.KeywordContinue:
					c.setOpOperand(jump.Addr, addr-jump.Addr) // continue -> end
				default:
					return logger.CompilerError(&block.Tok.Loc, "Unhandled jump-keyword: `%s`", keywords.Keyword2Str[jump.Keyword])
				}
			}
			op.Operand = block.Parent.Addr - addr // end -> while
			c.setOpOperand(block.Addr, end_diff)  // do -> end + 1 if condition is false
		case keywords.KeywordIf: // if-do-end
			// point all elif's to [end] + 1
			for _, jump := range block.Parent.Jumps {
				c.setOpOperand(jump.Addr, addr-jump.Addr+1)
			}
			// point last `do` to [end] + 1
			c.setOpOperand(block.Addr, addr-block.Addr+1)
		default:
			return logger.CompilerError(&token.Loc, "Unhandled parent block type while compiling `end` keyword")
		}
	case keywords.KeywordElse:
		// point all elif's and else to [end] + 1
		for _, jump := range block.Parent.Jumps {
			c.setOpOperand(jump.Addr, addr-jump.Addr+1)
		}
	case keywords.KeywordFunc: // func-do-end
		for _, jump := range block.Jumps {
			if jump.Keyword == keywords.KeywordReturn {
				c.setOpOperand(jump.Addr, addr-jump.Addr) // return -> end
			} else {
				return logger.CompilerError(&block.Tok.Loc, "Unhandled jump-keyword: `%s` in function", keywords.Keyword2Str[jump.Keyword])
			}
		}
		op.Typ, op.Data = operations.OpFuncEnd, scope.Name
	case keywords.KeywordCapture:
		count_drops := block.Data.(types.IntType)
		for i := 0; i < int(count_drops); i++ {
			v := scope.Captures.Pop()
			delete(scope.Names, v.Name)
		}
		c.pushOps(scope.Name, operations.Op{Typ: operations.OpDropCaptures, Token: token, Operand: block.Data.(types.IntType)})

		return nil
	default:
		return logger.CompilerError(&token.Loc, "Unhandled block start processing (processEndKeyword)")
	}
	c.pushOps(scope.Name, op)

	if block.Parent != nil {
		c.Blocks.Pop()
	}

	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileNamedBlock(token *tokens.Token, th *tokens.TokenHolder, scope *Scope) (name_token *tokens.Token, value Constant, err error) {
	if th.Empty() {
		err = logger.CompilerError(&token.Loc, "Expected `%s` name, but got nothing", token.Text)
		return
	}

	if !c.Blocks.Empty() && c.Blocks.Top().Typ != keywords.KeywordFunc {
		err = logger.CompilerError(
			&token.Loc,
			"%s-blocks should be used only inside function or global scope, but not in %s-block",
			token.Text, keywords.Keyword2Str[c.Blocks.Top().Typ],
		)
		return
	}

	name_token = th.GetNextToken()

	if name_token.Typ != tokens.TokenWord {
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

	value, err = c.constEval(name_token, th, scope)
	return
}

func (c *Compiler) checkConstTypes(token *tokens.Token, expected, actual datatypes.DataTypes) error {
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

func (c *Compiler) checkConstStackSize(token *tokens.Token, stack *ConstantStack, count int) error {
	if stack.Size() < count {
		return logger.CompilerError(
			&token.Loc, "Const stack size has %d elements but `%s` needs to have at least %d",
			stack.Size(), token.Text, count,
		)
	}
	return nil
}

func (c *Compiler) constEval(token *tokens.Token, th *tokens.TokenHolder, scope *Scope) (value Constant, err error) {
	const_stack := &ConstantStack{}

loop:
	for !th.Empty() {
		switch tok := th.GetNextToken(); tok.Typ {
		case tokens.TokenInt:
			const_stack.Push(Constant{Value: tok.Value.(types.IntType), Typ: datatypes.DataTypeInt})
		case tokens.TokenBool:
			const_stack.Push(Constant{Value: tok.Value.(types.IntType), Typ: datatypes.DataTypeBool})
		case tokens.TokenString:
			literal := tok.Value.(string)
			if literal_addr, exists := c.Ctx.StringsMap[literal]; !exists {
				err = logger.CompilerError(&tok.Loc, "Unknown string literal at compile-time const evaluation: `%s`", literal)
				return
			} else {
				const_stack.Push(Constant{Value: literal_addr, Typ: datatypes.DataTypePtr})
				const_stack.Push(Constant{Value: types.IntType(len(literal)), Typ: datatypes.DataTypeInt})
			}
		case tokens.TokenWord:
			if intrinsic, exists := intrinsics.Str2Intrinsic[tok.Text]; exists {
				switch intrinsic {
				case intrinsics.IntrinsicPlus, intrinsics.IntrinsicMinus, intrinsics.IntrinsicMul, intrinsics.IntrinsicBitAnd, intrinsics.IntrinsicBitXor, intrinsics.IntrinsicBitOr:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop()
					a := const_stack.Pop()
					if err = c.checkConstTypes(tok, datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}, datatypes.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: intrinsics.SafeArithmeticFunctions[intrinsic](a.Value, b.Value), Typ: datatypes.DataTypeInt})
				case intrinsics.IntrinsicBitNot:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					a := const_stack.Pop()
					if err = c.checkConstTypes(tok, datatypes.DataTypes{datatypes.DataTypeInt}, datatypes.DataTypes{a.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: ^a.Value, Typ: datatypes.DataTypeInt})
				case intrinsics.IntrinsicDiv:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop()
					if b.Value == 0 {
						err = logger.CompilerError(&tok.Loc, "Division by zero")
						return
					}
					a := const_stack.Pop()
					if err = c.checkConstTypes(tok, datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}, datatypes.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: a.Value / b.Value, Typ: datatypes.DataTypeInt})
				case intrinsics.IntrinsicMod:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop()
					if b.Value == 0 {
						err = logger.CompilerError(&tok.Loc, "Modulo by zero")
						return
					}
					a := const_stack.Pop()
					if err = c.checkConstTypes(tok, datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}, datatypes.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: a.Value % b.Value, Typ: datatypes.DataTypeInt})
				case intrinsics.IntrinsicShl:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop()
					if b.Value < 0 {
						err = logger.CompilerError(&tok.Loc, "Negative shift amount in `<<`: %d", b)
						return
					}
					a := const_stack.Pop()
					if err = c.checkConstTypes(tok, datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}, datatypes.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: a.Value << b.Value, Typ: datatypes.DataTypeInt})
				case intrinsics.IntrinsicShr:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop()
					if b.Value < 0 {
						err = logger.CompilerError(&tok.Loc, "Negative shift amount in `>>`: %d", b)
						return
					}
					a := const_stack.Pop()
					if err = c.checkConstTypes(tok, datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}, datatypes.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: a.Value >> b.Value, Typ: datatypes.DataTypeInt})
				case intrinsics.IntrinsicOffset:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					off := const_stack.Pop()
					if err = c.checkConstTypes(tok, datatypes.DataTypes{datatypes.DataTypeInt}, datatypes.DataTypes{off.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: c.Ctx.Offset, Typ: datatypes.DataTypeInt})
					c.Ctx.Offset += off.Value
				case intrinsics.IntrinsicGe, intrinsics.IntrinsicGt, intrinsics.IntrinsicLe, intrinsics.IntrinsicLt, intrinsics.IntrinsicEq, intrinsics.IntrinsicNe:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop()
					a := const_stack.Pop()
					if err = c.checkConstTypes(tok, datatypes.DataTypes{datatypes.DataTypeInt, datatypes.DataTypeInt}, datatypes.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: intrinsics.ComparableFunctions[intrinsic](a.Value, b.Value), Typ: datatypes.DataTypeBool})
				case intrinsics.IntrinsicLogicalAnd, intrinsics.IntrinsicLogicalOr:
					if err = c.checkConstStackSize(tok, const_stack, 2); err != nil {
						return
					}
					b := const_stack.Pop()
					a := const_stack.Pop()
					if err = c.checkConstTypes(tok, datatypes.DataTypes{datatypes.DataTypeBool, datatypes.DataTypeBool}, datatypes.DataTypes{a.Typ, b.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: intrinsics.LogicalFunctions[intrinsic](a.Value, b.Value), Typ: datatypes.DataTypeBool})
				case intrinsics.IntrinsicLogicalNot:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					a := const_stack.Pop()
					if err = c.checkConstTypes(tok, datatypes.DataTypes{datatypes.DataTypeBool}, datatypes.DataTypes{a.Typ}); err != nil {
						return
					}
					const_stack.Push(Constant{Value: intrinsics.B2I(!intrinsics.I2B(a.Value)), Typ: datatypes.DataTypeBool})
				case intrinsics.IntrinsicReset:
					const_stack.Push(Constant{Value: c.Ctx.Offset, Typ: datatypes.DataTypeInt})
					c.Ctx.Offset = 0
				case intrinsics.IntrinsicCastInt:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					a := const_stack.Pop()
					const_stack.Push(Constant{Value: a.Value, Typ: datatypes.DataTypeInt})
				case intrinsics.IntrinsicCastBool:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					a := const_stack.Pop()
					const_stack.Push(Constant{Value: a.Value, Typ: datatypes.DataTypeBool})
				case intrinsics.IntrinsicCastPtr:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					a := const_stack.Pop()
					const_stack.Push(Constant{Value: a.Value, Typ: datatypes.DataTypePtr})
				case intrinsics.IntrinsicCastFptr:
					if err = c.checkConstStackSize(tok, const_stack, 1); err != nil {
						return
					}
					a := const_stack.Pop()
					const_stack.Push(Constant{Value: a.Value, Typ: datatypes.DataTypeFptr})
				case intrinsics.IntrinsicDebug:
					values := make([]types.IntType, const_stack.Size())
					for i := range const_stack.Data {
						values[i] = const_stack.Data[i].Value
					}
					fmt.Println(logger.FormatNoneMsg(&tok.Loc, "Const stack values: %v", values))
				case intrinsics.IntrinsicTypeDebug:
					values := make(datatypes.DataTypes, const_stack.Size())
					for i := range const_stack.Data {
						values[i] = const_stack.Data[i].Typ
					}
					fmt.Println(logger.FormatNoneMsg(&tok.Loc, "Const stack types: %s", values))
				case intrinsics.IntrinsicAssert:
					if err = c.checkConstStackSize(tok, const_stack, 3); err != nil {
						return
					}
					size := const_stack.Pop()
					ptr := const_stack.Pop()
					cond := const_stack.Pop()
					if err = c.checkConstTypes(tok, datatypes.DataTypes{datatypes.DataTypeBool, datatypes.DataTypePtr, datatypes.DataTypeInt}, datatypes.DataTypes{cond.Typ, ptr.Typ, size.Typ}); err != nil {
						return
					}
					if cond.Value == 0 {
						for literal, literal_addr := range c.Ctx.StringsMap {
							if literal_addr == ptr.Value {
								err = logger.CompilerError(&tok.Loc, "ASSERT failed: `%s`", literal)
								return
							}
						}
						logger.CompilerCrash(&tok.Loc, "ASSERT failed, but string literal was not found")
					}
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
		case tokens.TokenKeyword:
			switch tok.Value.(keywords.KeywordType) {
			case keywords.KeywordEnd:
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

	value = const_stack.Pop()
	return
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileFuncSignature(token *tokens.Token, th *tokens.TokenHolder) (inputs, outputs datatypes.TypeStack, err error) {
	if th.Empty() {
		err = logger.CompilerError(&token.Loc, "Expected function signature, but got nothing")
		return
	}

	if n := th.NextToken(); n.Typ == tokens.TokenKeyword && !slices.Contains([]keywords.KeywordType{keywords.KeywordDo, keywords.KeywordColon}, n.Value.(keywords.KeywordType)) {
		err = logger.CompilerError(&n.Loc, "Expected function signature, but got `%s` keyword", n.Text)
		return
	}

	curr := &inputs
	for {
		if th.Empty() {
			err = logger.CompilerError(&token.Loc, "Unexpected end of tokens while compiling function signature")
			return
		}

		if n := th.NextToken(); n.Typ == tokens.TokenKeyword && n.Value.(keywords.KeywordType) == keywords.KeywordDo {
			break
		}

		switch next := th.GetNextToken(); next.Typ {
		case tokens.TokenWord:
			datatype, ok := next.Value.(datatypes.DataType)
			if !ok {
				err = logger.CompilerError(&next.Loc, "Unexpected word `%s` in function signature", next.Text)
				return
			}

			curr.Push(datatype)
		case tokens.TokenKeyword:
			if next.Value.(keywords.KeywordType) == keywords.KeywordColon {
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

func (c *Compiler) compileFuncDef(token *tokens.Token, th *tokens.TokenHolder) (name_token *tokens.Token, sig FuncSignature, err error) {
	if th.Empty() {
		err = logger.CompilerError(&token.Loc, "Expected `%s` name, but got nothing", token.Text)
		return
	}

	name_token = th.GetNextToken()
	if name_token.Typ != tokens.TokenWord {
		err = logger.CompilerError(&token.Loc, "Expected `%s` name to be a word, but got `%s`", token.Text, name_token.Text)
		return
	}

	if _, exists := intrinsics.Str2Intrinsic[name_token.Text]; exists {
		err = logger.CompilerError(&name_token.Loc, "Can not define function with the name of intrinsic")
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
	if do_token.Typ != tokens.TokenKeyword {
		err = logger.CompilerError(&do_token.Loc, "Expected `do` to start the function name, but got `%s`", do_token.Text)
		return
	}
	if do_token.Value.(keywords.KeywordType) != keywords.KeywordDo {
		err = logger.CompilerError(&do_token.Loc, "Expected keyword `do` to start the function name, but got `%s`", do_token.Text)
		return
	}
	sig = FuncSignature{Name: func_name, Inputs: inputs, Outputs: outputs}

	return
}

func (c *Compiler) compileFunc(token *tokens.Token, th *tokens.TokenHolder, scope *Scope) error {

	if scope.Name != GlobalScopeName {
		return logger.CompilerError(&token.Loc, "Cannot define functions inside a function %s", scope.Name)
	}
	if _, exists := c.Ctx.Funcs[EntryPointName]; exists {
		return logger.CompilerError(&token.Loc, "Cannot define functions after `%s`", EntryPointName)
	}

	inlined := false
	keyword := token.Value.(keywords.KeywordType)
	if keyword == keywords.KeywordInline {
		next := th.NextToken()
		if next.Typ != tokens.TokenKeyword || next.Value.(keywords.KeywordType) != keywords.KeywordFunc {
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

	if inlined && signature.Name == EntryPointName {
		return logger.CompilerError(&func_token.Loc, "Entry point can not be inlined")
	}

	// TODO: move to function
	new_scope := NewScope(signature.Name)
	new_scope.Names[signature.Name] = func_token
	c.Ctx.Scopes[signature.Name] = new_scope
	c.Ctx.GlobalScope().Names[signature.Name] = func_token

	c.prepareInlinedCache(inlined)

	func_addr := c.getCurrentAddr()
	c.Blocks.Push(NewBlock(func_addr, token, keywords.KeywordFunc, nil, nil))

	// declare function before compiling its body to allow recursion call
	f := &Function{
		Addr: func_addr, Sig: signature, Inlined: inlined,
	}
	c.Ctx.Funcs[signature.Name] = f

	c.pushOps(signature.Name, operations.Op{Token: token, Typ: operations.OpFuncBegin, Data: signature.Name})

	// do not use compileDoBlock, because in this case `do` should not compile to OpCondJump
	if err = c.compile(th, new_scope); err != nil {
		return err
	}

	// needed only for non-inlined functions
	c.setOpOperand(func_addr, new_scope.MemSize)            // OpFuncBegin $MEM - allocates   $MEM bytes in RAM
	c.setOpOperand(c.getCurrentAddr()-1, new_scope.MemSize) // OpFuncEnd $MEM   - deallocates $MEM bytes in RAM

	f.Ops = c.resetInlinedCache()
	c.Ctx.Funcs[signature.Name] = f

	if f.Inlined {
		c.pushOps(scope.Name, f.Ops...)
	}

	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) parseCapturedVal(th *tokens.TokenHolder, scope *Scope) (*CapturedVal, error) {
	if th.Empty() {
		return nil, logger.CompilerError(nil, "Expected captured value name but got nothing")
	}

	name_tok := th.GetNextToken()
	if name_tok.Typ != tokens.TokenWord {
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
	if typ_tok.Typ != tokens.TokenWord {
		return nil, logger.CompilerError(&typ_tok.Loc, "Expected captured value type but got `%s`", typ_tok.Text)
	}
	typ, ok := datatypes.Str2DataType[typ_tok.Text]
	if !ok {
		return nil, logger.CompilerError(&typ_tok.Loc, "Unknown type `%s`", typ_tok.Text)
	}
	if typ == datatypes.DataTypeAny {
		return nil, logger.CompilerError(&typ_tok.Loc, "`any` type is not allowed in capture list")
	}

	return &CapturedVal{Name: name, Typ: typ, Token: name_tok}, nil
}

func (c *Compiler) compileCaptureList(_ *tokens.Token, th *tokens.TokenHolder, scope *Scope) (*CaptureList, error) {
	captures := NewCaptureList()

	for !th.Empty() {
		t := th.NextToken()
		if t.Typ == tokens.TokenKeyword && t.Value.(keywords.KeywordType) == keywords.KeywordDo {
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

func (c *Compiler) compileCaptureKeyword(token *tokens.Token, th *tokens.TokenHolder, scope *Scope) error {
	if scope.Name == GlobalScopeName {
		return logger.CompilerError(&token.Loc, "Cannot capture variables inside global scope")
	}

	captures, err := c.compileCaptureList(token, th, scope)
	if err != nil {
		return err
	}

	val_types := datatypes.DataTypes{}
	for _, v := range captures.Vals {
		c.Ctx.Scopes[scope.Name].Captures.Push(v)
		c.Ctx.Scopes[scope.Name].Names[v.Name] = v.Token
		val_types = append(val_types, v.Typ)
	}

	cap_count := types.IntType(len(captures.Vals))
	c.pushOps(scope.Name, operations.Op{Typ: operations.OpCapture, Operand: cap_count, Token: token, Data: val_types})

	c.Blocks.Push(NewBlock(-1, token, keywords.KeywordCapture, cap_count, nil))
	if err := c.compile(th, scope); err != nil {
		return err
	}
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compileFptrOfKeyword(token *tokens.Token, th *tokens.TokenHolder, scope *Scope) error {
	if scope.Name == GlobalScopeName {
		return logger.CompilerError(&token.Loc, "Cannot get function pointer inside global scope")
	}

	if th.Empty() {
		return logger.CompilerError(&token.Loc, "Expected function name for getting its pointer, but got nothing")
	}

	func_token := th.GetNextToken()
	if func_token.Typ != tokens.TokenWord {
		return logger.CompilerError(&func_token.Loc, "Expected argument of `%s` to be a word, but got `%s`", token.Text, func_token.Text)
	}

	f, exists := c.Ctx.Funcs[func_token.Text]
	if !exists {
		return logger.CompilerError(&func_token.Loc, "Unknown function `%s`", func_token.Text)
	}

	c.pushOps(scope.Name, operations.Op{
		Typ: operations.OpPushFptr, Operand: f.Addr, Token: token, Data: f.Sig.Name,
	})
	return nil
}

func (c *Compiler) compileCallLikeKeyword(token *tokens.Token, th *tokens.TokenHolder, scope *Scope) error {
	if scope.Name == GlobalScopeName {
		return logger.CompilerError(&token.Loc, "Cannot `%s` function inside global scope", token.Text)
	}

	if th.Empty() {
		return logger.CompilerError(&token.Loc, "Expected function name as an argument for `%s`, but got nothing", token.Text)
	}

	func_token := th.GetNextToken()
	if func_token.Typ != tokens.TokenWord {
		return logger.CompilerError(&func_token.Loc, "Expected argument of `%s` to be a word, but got `%s`", token.Text, func_token.Text)
	}

	f, exists := c.Ctx.Funcs[func_token.Text]
	if !exists {
		return logger.CompilerError(&func_token.Loc, "Unknown function `%s`", func_token.Text)
	}

	c.pushOps(scope.Name, operations.Op{
		Typ: operations.OpCallLike, Data: f.Sig.Name, Token: token,
	})
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (c *Compiler) compile(th *tokens.TokenHolder, scope *Scope) error {
	for !th.Empty() {
		switch token := th.GetNextToken(); token.Typ {
		case tokens.TokenInt:
			if err := c.compileTokenInt(token, scope); err != nil {
				return err
			}
		case tokens.TokenString:
			if err := c.compileTokenString(token, scope); err != nil {
				return err
			}
		case tokens.TokenChar:
			if err := c.compileTokenChar(token, scope); err != nil {
				return err
			}
		case tokens.TokenBool:
			if err := c.compileTokenBool(token, scope); err != nil {
				return err
			}
		case tokens.TokenWord:
			name := token.Text

			if intrinsic, exists := intrinsics.Str2Intrinsic[name]; exists {
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
				if err := c.compileFuncCall(token, function, scope); err != nil {
					return err
				}
				continue
			}

			if index, exists := scope.GetCapturedValue(name); exists {
				c.pushOps(
					scope.Name,
					operations.Op{
						Token: token, Typ: operations.OpPushCaptured, Operand: index,
						Data: scope.Captures.Data[scope.Captures.Size()-1-int(index)].Typ,
					},
				)
				continue
			}

			return logger.CompilerError(&token.Loc, "Unknown word: `%s`", token.Text)
		case tokens.TokenKeyword:
			switch kw_type := token.Value.(keywords.KeywordType); kw_type {
			case keywords.KeywordIf:
				if err := c.compileIfBlock(token, th, scope); err != nil {
					return err
				}
			case keywords.KeywordElif:
				if err := c.compileElifBlock(token, th, scope); err != nil {
					return err
				}
			case keywords.KeywordElse:
				if err := c.compileElseBlock(token, th, scope); err != nil {
					return err
				}
			case keywords.KeywordEnd:
				return c.compileEndKeyword(token, scope) // return from compile() after processing `end`

			case keywords.KeywordWhile:
				if err := c.compileWhileBlock(token, th, scope); err != nil {
					return err
				}
			case keywords.KeywordDo:
				if err := c.compileDoBlock(token, th, scope); err != nil {
					return err
				}
			case keywords.KeywordBreak:
				if err := c.compileBreakKeyword(token, scope); err != nil {
					return err
				}
			case keywords.KeywordContinue:
				if err := c.compileContinueKeyword(token, scope); err != nil {
					return err
				}
			case keywords.KeywordReturn:
				if err := c.compileReturnKeyword(token, scope); err != nil {
					return err
				}

			case keywords.KeywordConst:
				tok, const_value, err := c.compileNamedBlock(token, th, scope)
				if err != nil {
					return err
				}
				scope.Consts[tok.Text] = const_value
			case keywords.KeywordAlloc:
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
				if alloc_size.Typ != datatypes.DataTypeInt {
					return logger.CompilerError(&tok.Loc, "Only `int` type is supported for `alloc` value, got `%s`", datatypes.DataType2Str[alloc_size.Typ])
				}

				if scope.Name != GlobalScopeName {
					scope.MemSize += alloc_size.Value
					scope.Allocs[tok.Text] = Allocation{
						Offset: scope.MemSize, Size: alloc_size.Value,
					}
				} else {
					scope.Allocs[tok.Text] = Allocation{
						Offset: scope.MemSize, Size: alloc_size.Value,
					}
					scope.MemSize += alloc_size.Value
				}

			case keywords.KeywordFunc, keywords.KeywordInline:
				if err := c.compileFunc(token, th, scope); err != nil {
					return err
				}

			case keywords.KeywordInclude:
				return logger.CompilerError(&token.Loc, "`include` keyword should not appear in here, probably there is a bug in a lexer")
			case keywords.KeywordCapture:
				if err := c.compileCaptureKeyword(token, th, scope); err != nil {
					return err
				}

			case keywords.KeywordFptrOf:
				if err := c.compileFptrOfKeyword(token, th, scope); err != nil {
					return err
				}
			case keywords.KeywordCallLike:
				if err := c.compileCallLikeKeyword(token, th, scope); err != nil {
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

func (c *Compiler) CompileTokens(th *tokens.TokenHolder, rts *vm.RuntimeSettings, s *settings.Settings) error {
	defer logger.Timeit(logger.ModuleCompiler, s.LogLevel)()

	// Set memory addresses for string literals
	c.Ctx.PreprocessStringLiterals(th, rts.StringLiteralsStart)

	th.Reset()
	if err := c.compile(th, c.Ctx.GlobalScope()); err != nil {
		return err
	}

	if !th.Empty() {
		return logger.CompilerError(nil, "[ERROR] No all tokens were processed")
	}

	if len(c.Blocks.Data) > 0 {
		top := c.Blocks.Data[len(c.Blocks.Data)-1]
		return logger.CompilerError(&top.Tok.Loc, "Unclosed `%s`-block", top.Tok.Text)
	}

	f, exists := c.Ctx.Funcs[EntryPointName]
	if !exists {
		return logger.CompilerError(nil, "No entry point found (function `%s` was not defined)", EntryPointName)
	}
	c.pushOps(GlobalScopeName, operations.Op{
		Typ: operations.OpCall, Operand: f.Addr - c.getCurrentAddr(), Data: f.Sig.Name,
		Token: &tokens.Token{
			Typ: tokens.TokenWord, Text: EntryPointName, Value: EntryPointName,
			Loc: utils.Location{Filepath: "", Line: -1, Column: -1},
		},
	})

	if !(f.Sig.Inputs.Size() == 0 && f.Sig.Outputs.Size() == 1 && f.Sig.Outputs.Top() == datatypes.DataTypeInt) {
		logger.CompilerCrash(
			&c.Ops[f.Addr].Token.Loc,
			"Expected entry point `%s` to have zero inputs and `int` as an output, "+
				"but got input=%s and output=%s",
			f.Sig.Name, f.Sig.Inputs.Data, f.Sig.Outputs.Data,
		)
	}

	// Set runtime parameters
	rts.EntryPointAddr = c.EntryPointAddr()
	rts.GlobalMemorySize = c.Ctx.GlobalScope().MemSize
	rts.StringLiterals = &c.Ctx.StringsMap
	rts.OpsCount = c.getCurrentAddr()

	return nil
}

// ---------------------------------------------------------------------------------------------------------------------
