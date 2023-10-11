package vm

import (
	"Gorth/interpreter/lexer"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/types"
	"Gorth/interpreter/utils"
	"fmt"
	"strings"
)

type context_type int

const (
	context_type_global context_type = iota
	context_type_func
	context_type_if
	context_type_while
)

// ---------------------------------------------------------------------------------------------------------------------

type TypeChecker struct {
	Ctx *CompileTimeContext
}

func NewTypeChecker(ctx *CompileTimeContext) *TypeChecker {
	return &TypeChecker{Ctx: ctx}
}

// ---------------------------------------------------------------------------------------------------------------------

type TypeCheckerOutputs struct {
	Index  int
	Stacks []utils.Stack // slice of possible type stacks
}

func NewTypeCheckerOutputs() *TypeCheckerOutputs {
	return &TypeCheckerOutputs{Index: -1, Stacks: make([]utils.Stack, 0)}
}

func (tco *TypeCheckerOutputs) SameOutputs() bool {
	if len(tco.Stacks) == 0 {
		logger.Crash(nil, "[TypeCheck] FATAL ERROR: TypeCheckerOutputs has empty Stacks field")
	}
	for i := 0; i < len(tco.Stacks)-1; i++ {
		if !utils.StacksAreEqual[lexer.DataType](&tco.Stacks[i], &tco.Stacks[i+1]) {
			return false
		}
	}
	return true
}

func (tco *TypeCheckerOutputs) FormatStacks() string {
	items := make([]string, 0)
	for _, s := range tco.Stacks {
		items = append(items, fmt.Sprintf("%s", s.Data))
	}
	return strings.Join(items, "\n")
}

func OutputsAreSame(f, s *TypeCheckerOutputs) bool {
	if f.Index != s.Index {
		return false
	}
	for i := range f.Stacks {
		if !utils.StacksAreEqual[lexer.DataType](&f.Stacks[i], &s.Stacks[i]) {
			return false
		}
	}
	return true
}

// ---------------------------------------------------------------------------------------------------------------------

type TypeCheckerContext struct {
	Typ        context_type
	Stack      utils.Stack
	Terminated bool
	Outputs    TypeCheckerOutputs
}

func NewTypeCheckerContext(t context_type) *TypeCheckerContext {
	return &TypeCheckerContext{Typ: t, Stack: utils.Stack{}, Outputs: *NewTypeCheckerOutputs(), Terminated: false}
}

func (tcc *TypeCheckerContext) Clone(t context_type) *TypeCheckerContext {
	clone := NewTypeCheckerContext(t)
	clone.Stack = *tcc.Stack.Copy()
	return clone
}

func ContextsAreSame(f, s *TypeCheckerContext) bool {
	return f.Typ == s.Typ && utils.StacksAreEqual[lexer.DataType](&f.Stack, &s.Stack) && OutputsAreSame(&f.Outputs, &s.Outputs) && f.Terminated == s.Terminated
}

// ---------------------------------------------------------------------------------------------------------------------

type TypeCheckerContextStack struct {
	stack utils.Stack
}

func NewTypeCheckerContextStack() *TypeCheckerContextStack {
	return &TypeCheckerContextStack{}
}

func (cs *TypeCheckerContextStack) Push(c *TypeCheckerContext) {
	cs.stack.Push(c)
}

func (cs *TypeCheckerContextStack) Pop() *TypeCheckerContext {
	if cs.stack.Empty() {
		logger.Crash(nil, "[TypeCheck] FATAL ERROR: pop from empty TypeCheckerContextStack")
	}
	c, ok := cs.stack.Pop().(*TypeCheckerContext)
	if !ok {
		logger.Crash(nil, "[TypeCheck] FATAL ERROR: pop item from TypeCheckerContextStack has bad type")
	}
	return c
}

func (cs *TypeCheckerContextStack) Top() *TypeCheckerContext {
	if cs.stack.Empty() {
		logger.Crash(nil, "[TypeCheck] FATAL ERROR: top from empty TypeCheckerContextStack")
	}
	c, ok := cs.stack.Top().(*TypeCheckerContext)
	if !ok {
		logger.Crash(nil, "[TypeCheck] FATAL ERROR: top item from TypeCheckerContextStack has bad type")
	}
	return c
}

func (cs *TypeCheckerContextStack) Size() int {
	return cs.stack.Size()
}

func (cs *TypeCheckerContextStack) GetContext(t context_type) *TypeCheckerContext {
	for j := cs.Size() - 1; j >= 0; j-- {
		ctx := cs.stack.Data[j].(*TypeCheckerContext)
		if ctx.Typ == t {
			return ctx
		}
	}
	logger.Crash(nil, "[TypeCheck] FATAL ERROR: Cannot find closest context for %d", t)
	return nil
}

func (cs *TypeCheckerContextStack) Clone() *TypeCheckerContextStack {
	return &TypeCheckerContextStack{stack: *cs.stack.Copy()}
}

/*
func ContextStacksAreSame(f, s *TypeCheckerContextStack) bool {
	if f.Size() != s.Size() {
		return false
	}
	if f.Size() == 0 {
		return true
	}
	for i := range f.stack.Data {
		if !ContextsAreSame(f.stack.Data[i].(*TypeCheckerContext), s.stack.Data[i].(*TypeCheckerContext)) {
			return false
		}
	}
	return true
}
*/

// ---------------------------------------------------------------------------------------------------------------------
// TODO: Update error messages: provide locations
// ---------------------------------------------------------------------------------------------------------------------

func (tc *TypeChecker) enoughArgsCount(stack *utils.Stack, count int, token *lexer.Token) (err error) {
	if stack.Size() < count {
		err = logger.TypeCheckerError(&token.Loc, "Not enough arguments for `%s`", token.Text)
	}
	return
}

func (tc *TypeChecker) popType(op *Op, stack *utils.Stack, expected lexer.DataType) error {
	if err := tc.enoughArgsCount(stack, 1, &op.OpToken); err != nil {
		return err
	}

	actual, ok := stack.Top().(lexer.DataType)
	if !ok {
		return logger.TypeCheckerError(&op.OpToken.Loc, "Cannot convert stack item to DataType")
	}

	if actual != expected {
		return logger.TypeCheckerError(
			&op.OpToken.Loc, "Expected argument of type `%s` but got `%s`. Current stack: %s",
			lexer.DataTypeName[expected], lexer.DataTypeName[actual], stack.Data,
		)
	}
	stack.Pop()

	return nil
}

func (tc *TypeChecker) popTypes(op *Op, stack, expected *utils.Stack) error {
	if err := tc.enoughArgsCount(stack, expected.Size(), &op.OpToken); err != nil {
		return err
	}

	for i := expected.Size() - 1; i >= 0; i-- {
		t := expected.Data[i].(lexer.DataType)
		if err := tc.popType(op, stack, t); err != nil {
			return err
		}
	}
	return nil
}

func (tc *TypeChecker) popTypeContract(op *Op, stack *utils.Stack, expected lexer.DataType) (actual lexer.DataType, err error) {
	actual, ok := stack.Pop().(lexer.DataType)
	if !ok {
		err = logger.TypeCheckerError(&op.OpToken.Loc, "Cannot convert stack item to DataType")
		return
	}

	if expected != lexer.DataTypeAny && actual != expected {
		err = logger.TypeCheckerError(
			&op.OpToken.Loc, "Expected argument of type `%s` but got `%s`. Current stack: %s",
			lexer.DataTypeName[expected], lexer.DataTypeName[actual], stack.Data,
		)
	}
	return
}

func (tc *TypeChecker) popTypesContract(op *Op, stack *utils.Stack, contract *lexer.Contract) (d lexer.DataTypes, err error) {
	if err = tc.enoughArgsCount(stack, contract.Inputs.Size(), &op.OpToken); err != nil {
		return
	}

	d = make(lexer.DataTypes, contract.Inputs.Size())
	for i := contract.Inputs.Size() - 1; i >= 0; i-- {
		e := contract.Inputs.Data[i].(lexer.DataType)
		d[i], err = tc.popTypeContract(op, stack, e)
		if err != nil {
			return
		}
	}
	return
}

func (tc *TypeChecker) typeCheckOutputs(op *Op, outputs lexer.DataTypes, expected *utils.Stack) error {
	if len(outputs) != expected.Size() {
		return logger.TypeCheckerError(
			&op.OpToken.Loc, "Outputs for `%s` don't fit to its contract(expected: %s actual: %s)",
			op.OpToken.Text, expected.Data, outputs,
		)
	}

	for i, a := range outputs {
		e := expected.Data[i].(lexer.DataType)
		if e != lexer.DataTypeAny && e != a {
			return logger.TypeCheckerError(
				&op.OpToken.Loc, "Output arg #%d for `%s` don't fit to its contract(expected: %s actual: %s)",
				i, op.OpToken.Text, lexer.DataTypeName[e], lexer.DataTypeName[a],
			)
		}
	}
	return nil
}

func (tc *TypeChecker) typeCheckIntrinsic(op *Op, stack *utils.Stack, i lexer.IntrinsicType, ctx *TypeCheckerContext) error {
	contract, err_msg := lexer.GetIntrinsicContract(i)
	if err_msg != "" {
		return logger.TypeCheckerError(&op.OpToken.Loc, "No contract for intrinsic found: %s", err_msg)
	}

	inputs, err := tc.popTypesContract(op, stack, contract)
	if err != nil {
		return err
	}

	logic, err_msg := lexer.GetIntrinsicLogic(i)
	if err_msg != "" {
		return logger.TypeCheckerError(&op.OpToken.Loc, "No logic for intrinsic found: %s", err_msg)
	}

	f := lexer.DefaultCustomFunc
	if i == lexer.IntrinsicTypeDebug {
		f = func() lexer.DataTypes {
			fmt.Println(logger.FormatInfoMsg(&op.OpToken.Loc, "[TypeCheck DEBUG] stack: %s", ctx.Stack.Data))
			return lexer.DataTypes{}
		}
	}

	outputs := logic(i, contract, inputs, f)
	if err := tc.typeCheckOutputs(op, outputs, contract.Outputs); err != nil {
		return err
	}

	for _, item := range outputs {
		stack.Push(item)
	}
	return nil
}

func (tc *TypeChecker) typeCheckFunc(ops *[]Op, i int, contextStack *TypeCheckerContextStack) (index int, err error) {
	index = -1

	ctx := contextStack.Top().Clone(context_type_func)
	contextStack.Push(ctx)

	func_name := (*ops)[i].Data.(string)
	f, exists := tc.Ctx.Funcs[func_name]
	if !exists {
		err = logger.TypeCheckerError(&(*ops)[i].OpToken.Loc, "No such function: `%s`", func_name)
		return
	}
	for _, t := range f.Sig.Inputs.Data {
		ctx.Stack.Push(t.(lexer.DataType))
	}

	if err = tc.typeCheck(ops, i+1, contextStack); err != nil {
		return
	}

	// TODO: check that contextStack preserves its size and popped item has Typ = context_type_func
	outputs := contextStack.Pop().Outputs
	if !outputs.SameOutputs() {
		err = logger.TypeCheckerError(
			nil, "Function `%s` could finish with different output types:\n%s",
			func_name, outputs.FormatStacks(),
		)
		return
	}

	if !utils.StacksAreEqual[lexer.DataType](&f.Sig.Outputs, &outputs.Stacks[0]) {
		err = logger.TypeCheckerError(
			nil, "Function `%s` does not fit to its signature. Expected `%s` but got `%s`",
			func_name, f.Sig.Outputs.Data, outputs.Stacks[0].Data,
		)
		return
	}
	index = outputs.Index + 1

	return
}

func (tc *TypeChecker) typeCheckWhileBlock(ops *[]Op, i int, contextStack *TypeCheckerContextStack) (int, error) {
	index := -1

	ctx := contextStack.Top().Clone(context_type_while)
	contextStack.Push(ctx)

	loc := &(*ops)[i].OpToken.Loc

	// process while case
	if err := tc.typeCheck(ops, i+1, contextStack); err != nil {
		return i, err
	}
	i = contextStack.Top().Outputs.Index

	if err := tc.typeCheck(ops, i+1, contextStack); err != nil {
		return i, err
	}
	// i = contextStack.Top().Outputs.Index

	if contextStack.Top().Terminated {
		index = contextStack.Top().Outputs.Index
		contextStack.Pop()
		contextStack.Top().Terminated = true
	} else {
		top := contextStack.Pop()

		if !top.Outputs.SameOutputs() {
			return index, logger.TypeCheckerError(
				loc, "While-loop could finish with different stack results:\n%s",
				top.Outputs.FormatStacks(),
			)
		}

		if !utils.StacksAreEqual[lexer.DataType](&contextStack.Top().Stack, &top.Outputs.Stacks[0]) {
			return index, logger.TypeCheckerError(
				loc, "While-loop iteration changes stack. Expected: %s actual: %s",
				contextStack.Top().Stack.Data, top.Stack.Data,
			)
		}
		index = top.Outputs.Index + 1
	}
	return index, nil
}

func (tc *TypeChecker) typeCheckIfBlock(ops *[]Op, i int, contextStack *TypeCheckerContextStack) (int, error) {
	index := -1

	loc := &(*ops)[i].OpToken.Loc

	// process if case
	if err := tc.typeCheck(ops, i+1, contextStack); err != nil {
		return i, err
	}
	top := contextStack.Top()
	i = top.Outputs.Index

	// now i points to the `do`-operation
	do_op := &(*ops)[i]
	do_op_operand := int(do_op.Operand.(types.IntType))
	j := i + do_op_operand - 1

	switch (*ops)[j].Data.(string) {
	case "else": // if-do-else-end, so if-branch should do the same stack as else-branch
		true_stack := contextStack.Clone()
		ctx1 := top.Clone(context_type_if)
		true_stack.Push(ctx1)

		false_stack := contextStack.Clone()
		ctx2 := top.Clone(context_type_if)
		false_stack.Push(ctx2)

		if err := tc.typeCheck(ops, i+1, true_stack); err != nil {
			return index, err
		}
		if err := tc.typeCheck(ops, j+1, false_stack); err != nil {
			return index, err
		}

		if false_stack.Top().Terminated {
			if true_stack.Top().Terminated {
				// TODO: check only top contexts maybe?
				if !ContextsAreSame(true_stack.Top(), false_stack.Top()) {
					return index, logger.TypeCheckerError(loc, "`true` and `false` branch results of if-else-block does not match (both has return)")
				}
				top.Stack = false_stack.Top().Stack
				index = false_stack.Top().Outputs.Index // go to OpFuncEnd
				top.Terminated = true
			} else {
				top.Stack = true_stack.Top().Stack
				index = j + int((*ops)[j].Operand.(types.IntType)) // go to end+1
			}
		} else {
			if !true_stack.Top().Terminated && !ContextsAreSame(true_stack.Top(), false_stack.Top()) {
				return index, logger.TypeCheckerError(loc, "`true` and `false` branch results of if-else-block does not match")
			}
			top.Stack = false_stack.Top().Stack
			index = false_stack.Top().Outputs.Index + 1 // go to end+1
		}
		return index, nil
	case "end": // if-do-end, so if-branch should preserve the stack
		true_stack := contextStack.Clone()
		ctx1 := top.Clone(context_type_if)
		true_stack.Push(ctx1)

		false_stack := contextStack.Clone()
		ctx2 := top.Clone(context_type_if)
		ctx2.Outputs.Stacks = append(ctx2.Outputs.Stacks, ctx2.Stack)
		ctx2.Outputs.Index = j
		false_stack.Push(ctx2)

		if err := tc.typeCheck(ops, i+1, true_stack); err != nil {
			return index, err
		}

		if !true_stack.Top().Terminated && !ContextsAreSame(true_stack.Top(), false_stack.Top()) {
			return index, logger.TypeCheckerError(loc, "`true` branch results of if-block differs from `false`")
		}
		top.Stack = false_stack.Top().Stack
		index = j + 1 // go to end+1
		return index, nil
	default:
		return index, logger.TypeCheckerError(loc, "`do` does not point neither to `else`, not to `end`, probably bug in compiler")
	}
}

func (tc *TypeChecker) typeCheck(ops *[]Op, start int, contextStack *TypeCheckerContextStack) ( /*o *TypeCheckerOutputs, */ err error) {
	ctx := contextStack.Top()

	for i := start; i < len(*ops); {
		op := &(*ops)[i]

		switch op.Typ {
		case OpPushInt:
			ctx.Stack.Push(lexer.DataTypeInt)
			i++
		case OpPushBool:
			ctx.Stack.Push(lexer.DataTypeBool)
			i++
		case OpPushPtr:
			ctx.Stack.Push(lexer.DataTypePtr)
			i++
		case OpIntrinsic:
			if err = tc.typeCheckIntrinsic(op, &ctx.Stack, op.Operand.(lexer.IntrinsicType), ctx); err != nil {
				return
			}
			/*
				switch intrinsic := op.Operand.(lexer.IntrinsicType); intrinsic {
				case lexer.IntrinsicEq, lexer.IntrinsicNe, lexer.IntrinsicGe, lexer.IntrinsicGt, lexer.IntrinsicLe, lexer.IntrinsicLt:
					// if err = tc.popTypes(op, &ctx.Stack, utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt})); err != nil {
					// 	return
					// }
					// ctx.Stack.Push(lexer.DataTypeBool)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicPlus, lexer.IntrinsicMinus, lexer.IntrinsicMul, lexer.IntrinsicDiv, lexer.IntrinsicMod,
					lexer.IntrinsicBitAnd, lexer.IntrinsicBitOr, lexer.IntrinsicBitXor,
					lexer.IntrinsicShl, lexer.IntrinsicShr:
					// if err = tc.popTypes(op, &ctx.Stack, utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypeInt})); err != nil {
					// 	return
					// }
					// ctx.Stack.Push(lexer.DataTypeInt)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicBitNot:
					// if err = tc.popType(op, &ctx.Stack, lexer.DataTypeInt); err != nil {
					// 	return
					// }
					// ctx.Stack.Push(lexer.DataTypeInt)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicLogicalAnd, lexer.IntrinsicLogicalOr:
					// if err = tc.popTypes(op, &ctx.Stack, utils.NewStack([]lexer.DataType{lexer.DataTypeBool, lexer.DataTypeBool})); err != nil {
					// 	return
					// }
					// ctx.Stack.Push(lexer.DataTypeBool)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicLogicalNot:
					// if err = tc.popType(op, &ctx.Stack, lexer.DataTypeBool); err != nil {
					// 	return
					// }
					// ctx.Stack.Push(lexer.DataTypeBool)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicDup:
					// if err = tc.enoughArgsCount(&ctx.Stack, 1, &op.OpToken); err != nil {
					// 	return
					// }
					// ctx.Stack.Push(ctx.Stack.Top().(lexer.DataType))
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicSwap:
					// if err = tc.enoughArgsCount(&ctx.Stack, 2, &op.OpToken); err != nil {
					// 	return
					// }
					// b := ctx.Stack.Pop().(lexer.DataType)
					// a := ctx.Stack.Pop().(lexer.DataType)
					// ctx.Stack.Push(b)
					// ctx.Stack.Push(a)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicDrop:
					// if err = tc.enoughArgsCount(&ctx.Stack, 1, &op.OpToken); err != nil {
					// 	return
					// }
					// ctx.Stack.Pop()
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicOver:
					// if err = tc.enoughArgsCount(&ctx.Stack, 2, &op.OpToken); err != nil {
					// 	return
					// }
					// a := ctx.Stack.Data[ctx.Stack.Size()-2].(lexer.DataType)
					// ctx.Stack.Push(a)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicRot:
					// if err = tc.enoughArgsCount(&ctx.Stack, 3, &op.OpToken); err != nil {
					// 	return
					// }
					// c := ctx.Stack.Pop().(lexer.DataType)
					// b := ctx.Stack.Pop().(lexer.DataType)
					// a := ctx.Stack.Pop().(lexer.DataType)
					// ctx.Stack.Push(b)
					// ctx.Stack.Push(c)
					// ctx.Stack.Push(a)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicPuti:
					// if err = tc.popType(op, &ctx.Stack, lexer.DataTypeInt); err != nil {
					// 	return
					// }
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}

				case lexer.IntrinsicDebug:
					// nothing to do
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicTypeDebug:
					// fmt.Println(logger.FormatInfoMsg(&op.OpToken.Loc, "[TypeCheck DEBUG] stack: %s", ctx.Stack.Data))
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}

				case lexer.IntrinsicLoad8, lexer.IntrinsicLoad16, lexer.IntrinsicLoad32, lexer.IntrinsicLoad64:
					// if err = tc.popType(op, &ctx.Stack, lexer.DataTypePtr); err != nil {
					// 	return
					// }
					// ctx.Stack.Push(lexer.DataTypeInt)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicStore8, lexer.IntrinsicStore16, lexer.IntrinsicStore32, lexer.IntrinsicStore64:
					// if err = tc.popTypes(op, &ctx.Stack, utils.NewStack([]lexer.DataType{lexer.DataTypeInt, lexer.DataTypePtr})); err != nil {
					// 	return
					// }
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicArgc:
					// ctx.Stack.Push(lexer.DataTypeInt)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicArgv, lexer.IntrinsicEnv:
					// ctx.Stack.Push(lexer.DataTypePtr)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicSyscall1:
					// if err = tc.enoughArgsCount(&ctx.Stack, 2, &op.OpToken); err != nil {
					// 	return
					// }
					// ctx.Stack.Pop()
					// ctx.Stack.Pop()
					// ctx.Stack.Push(lexer.DataTypeInt)
					// ctx.Stack.Push(lexer.DataTypeInt)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicSyscall3:
					// if err = tc.enoughArgsCount(&ctx.Stack, 4, &op.OpToken); err != nil {
					// 	return
					// }
					// ctx.Stack.Pop()
					// ctx.Stack.Pop()
					// ctx.Stack.Pop()
					// ctx.Stack.Pop()
					// ctx.Stack.Push(lexer.DataTypeInt)
					// ctx.Stack.Push(lexer.DataTypeInt)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}

				case lexer.IntrinsicCastBool:
					// if err = tc.enoughArgsCount(&ctx.Stack, 1, &op.OpToken); err != nil {
					// 	return
					// }
					// ctx.Stack.Pop()
					// ctx.Stack.Push(lexer.DataTypeBool)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicCastInt:
					// if err = tc.enoughArgsCount(&ctx.Stack, 1, &op.OpToken); err != nil {
					// 	return
					// }
					// ctx.Stack.Pop()
					// ctx.Stack.Push(lexer.DataTypeInt)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}
				case lexer.IntrinsicCastPtr:
					// if err = tc.enoughArgsCount(&ctx.Stack, 1, &op.OpToken); err != nil {
					// 	return
					// }
					// ctx.Stack.Pop()
					// ctx.Stack.Push(lexer.DataTypePtr)
					if err = tc.typeCheckIntrinsic(op, &ctx.Stack, intrinsic, ctx); err != nil {
						return
					}

				default:
					err = logger.TypeCheckerError(&op.OpToken.Loc, "Unexpected intrinsic `%s`", op.OpToken.Text)
					return
				}
			*/
			i++

		case OpJump:
			// process if-else-end, while-break-continue-end and return differently
			block_type := op.Data.(string)
			switch block_type {
			case "return":
				func_ctx := contextStack.GetContext(context_type_func)
				func_ctx.Outputs.Stacks = append(func_ctx.Outputs.Stacks, *ctx.Stack.Copy())
				func_ctx.Outputs.Index = i + int(op.Operand.(types.IntType))

				if ctx.Typ == context_type_if || ctx.Typ == context_type_while {
					ctx.Terminated = true
					ctx.Outputs.Index = func_ctx.Outputs.Index
				}
				return nil

			case "end":
				ctx.Outputs.Stacks = append(ctx.Outputs.Stacks, *ctx.Stack.Copy())
				ctx.Outputs.Index = i
				return nil
			case "break", "continue":
				while_ctx := contextStack.GetContext(context_type_while)
				while_ctx.Outputs.Stacks = append(while_ctx.Outputs.Stacks, *ctx.Stack.Copy())

				if block_type == "break" {
					while_ctx.Outputs.Index = i + int(op.Operand.(types.IntType)) - 1
				} else {
					while_ctx.Outputs.Index = i + int(op.Operand.(types.IntType))
				}

				if ctx.Typ == context_type_if {
					ctx.Terminated = true
					ctx.Outputs.Index = while_ctx.Outputs.Index
				}
				return nil

			case "while":
				i, err = tc.typeCheckWhileBlock(ops, i, contextStack)
				if err != nil {
					return
				}

			case "if":
				i, err = tc.typeCheckIfBlock(ops, i, contextStack)
				if err != nil {
					return
				}
			case "else":
				ctx.Outputs.Stacks = append(ctx.Outputs.Stacks, *ctx.Stack.Copy())
				ctx.Outputs.Index = i + int(op.Operand.(types.IntType)) - 1
				return nil
			default:
				return logger.TypeCheckerError(&op.OpToken.Loc, "Type checking for %s (OpJump) is not implemented yet", block_type)

			}

		case OpCondJump:
			if err = tc.popType(op, &ctx.Stack, lexer.DataTypeBool); err != nil {
				return
			}
			ctx.Outputs.Index = i
			return

		case OpFuncBegin:
			i, err = tc.typeCheckFunc(ops, i, contextStack)
			if err != nil {
				return
			}
		case OpFuncEnd:
			ctx.Outputs.Stacks = append(ctx.Outputs.Stacks, *ctx.Stack.Copy())
			ctx.Outputs.Index = i
			return nil

		case OpCall:
			f := tc.Ctx.Funcs[op.Data.(string)]
			if err = tc.popTypes(op, &ctx.Stack, &f.Sig.Inputs); err != nil {
				return
			}
			for _, t := range f.Sig.Outputs.Data {
				ctx.Stack.Push(t.(lexer.DataType))
			}
			i++

		case OpPushGlobalAlloc, OpPushLocalAlloc:
			ctx.Stack.Push(lexer.DataTypePtr)
			i++
		default:
			err = logger.TypeCheckerError(&op.OpToken.Loc, "Type check for `%s` op is not implemented yet", OpName[op.Typ])
			return
		}
	}

	top := contextStack.Top()
	if top.Typ != context_type_global || contextStack.Size() != 1 {
		return logger.TypeCheckerError(nil, "FATAL ERROR: outof ops but got non-empty contextStack or non-global context.Typ")
	}
	top.Outputs.Stacks = append(top.Outputs.Stacks, *ctx.Stack.Copy())
	top.Outputs.Index = len(*ops)

	return nil
}

func (tc *TypeChecker) TypeCheckProgram(ops *[]Op) error {

	contextStack := NewTypeCheckerContextStack()
	context := NewTypeCheckerContext(context_type_global)
	contextStack.Push(context)

	err := tc.typeCheck(ops, 0, contextStack)
	if err != nil {
		return err
	}

	context = contextStack.Pop()
	// if context.Outputs.Index != len(*ops) {
	// 	return logger.FormatErrMsg(nil, "[TypeCheck] Not all ops were typechecked (have %d in total, but end=%d)", len(*ops), context.Outputs.Index)
	// }
	if len(context.Outputs.Stacks) != 1 {
		return logger.TypeCheckerError(nil, "There should be only one stack in TypeCheckerOutputs but got %d", len(context.Outputs.Stacks))
	}

	s := context.Outputs.Stacks[0]
	if !(context.Typ == context_type_global && s.Size() == 1 && s.Top().(lexer.DataType) == lexer.DataTypeInt) {
		return logger.TypeCheckerError(nil, "Expected typecheck stack to contain `int` as exit code at the end, but got %s", s.Data)
	}
	return nil
}
