package typechecker

import (
	"Gorth/interpreter/compiler"
	"Gorth/interpreter/datatypes"
	"Gorth/interpreter/intrinsics"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/operations"
	"Gorth/interpreter/settings"
	"Gorth/interpreter/tokens"
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
	Ctx     *compiler.CompileTimeContext
	enabled bool
}

func NewTypeChecker(enabled bool) *TypeChecker {
	return &TypeChecker{enabled: enabled}
}

// ---------------------------------------------------------------------------------------------------------------------

type TypeCheckerJumpResult struct {
	Stack datatypes.TypeStack
	Token *tokens.Token
}

// ---------------------------------------------------------------------------------------------------------------------

type TypeCheckerOutputs struct {
	Index   int
	Results []TypeCheckerJumpResult // slice of possible type stacks
}

func NewTypeCheckerOutputs() *TypeCheckerOutputs {
	return &TypeCheckerOutputs{Index: -1, Results: make([]TypeCheckerJumpResult, 0)}
}

func (tco *TypeCheckerOutputs) SameOutputs() bool {
	if len(tco.Results) == 0 {
		logger.TypeCheckerCrash(nil, "FATAL ERROR: TypeCheckerOutputs has empty Results field")
	}
	for i := 0; i < len(tco.Results)-1; i++ {
		if !utils.StacksAreEqual[datatypes.DataType](&tco.Results[i].Stack, &tco.Results[i+1].Stack) {
			return false
		}
	}
	return true
}

func (tco *TypeCheckerOutputs) FormatStacks(indent string) string {
	items := make([]string, 0)
	for _, s := range tco.Results {
		items = append(items, fmt.Sprintf("%s%s: %s", indent, logger.FormatLoc(&s.Token.Loc), s.Stack.Data))
	}
	return strings.Join(items, "\n")
}

func OutputsAreSame(f, s *TypeCheckerOutputs) bool {
	for i := range f.Results {
		if !utils.StacksAreEqual[datatypes.DataType](&f.Results[i].Stack, &s.Results[i].Stack) {
			return false
		}
	}
	return true
}

// ---------------------------------------------------------------------------------------------------------------------

type TypeCheckerContext struct {
	Typ        context_type
	Stack      datatypes.TypeStack
	Captures   datatypes.TypeStack
	Terminated bool
	Outputs    TypeCheckerOutputs
}

func NewTypeCheckerContext(t context_type) *TypeCheckerContext {
	return &TypeCheckerContext{
		Typ: t, Stack: datatypes.TypeStack{}, Captures: datatypes.TypeStack{},
		Outputs: *NewTypeCheckerOutputs(), Terminated: false,
	}
}

func (tcc *TypeCheckerContext) Clone(t context_type) *TypeCheckerContext {
	clone := NewTypeCheckerContext(t)
	clone.Stack = *tcc.Stack.Copy()
	clone.Captures = *tcc.Captures.Copy()
	return clone
}

func ContextsAreSame(f, s *TypeCheckerContext) bool {
	return f.Typ == s.Typ && utils.StacksAreEqual[datatypes.DataType](&f.Stack, &s.Stack) && OutputsAreSame(&f.Outputs, &s.Outputs) && f.Terminated == s.Terminated
}

func FormatContextSliceOutputs(s []*TypeCheckerContext, indent string) string {
	res := make([]string, 0)
	for _, c := range s {
		res = append(res, c.Outputs.FormatStacks(indent))
	}
	return strings.Join(res, "\n")
}

// ---------------------------------------------------------------------------------------------------------------------

type typeCheckerContextStack = utils.Stack[*TypeCheckerContext]

type TypeCheckerContextStack struct {
	stack typeCheckerContextStack
}

func NewTypeCheckerContextStack() *TypeCheckerContextStack {
	return &TypeCheckerContextStack{}
}

func (cs *TypeCheckerContextStack) Push(c *TypeCheckerContext) {
	cs.stack.Push(c)
}

func (cs *TypeCheckerContextStack) Pop() *TypeCheckerContext {
	if cs.stack.Empty() {
		logger.TypeCheckerCrash(nil, "Pop from empty TypeCheckerContextStack")
	}
	return cs.stack.Pop()
}

func (cs *TypeCheckerContextStack) Top() *TypeCheckerContext {
	if cs.stack.Empty() {
		logger.TypeCheckerCrash(nil, "Top from empty TypeCheckerContextStack")
	}
	return cs.stack.Top()
}

func (cs *TypeCheckerContextStack) Size() int {
	return cs.stack.Size()
}

func (cs *TypeCheckerContextStack) GetContext(t context_type) *TypeCheckerContext {
	for j := cs.Size() - 1; j >= 0; j-- {
		ctx := cs.stack.Data[j]
		if ctx.Typ == t {
			return ctx
		}
	}
	logger.TypeCheckerCrash(nil, "Cannot find closest context for %d", t)
	return nil
}

func (cs *TypeCheckerContextStack) Clone() *TypeCheckerContextStack {
	return &TypeCheckerContextStack{stack: *cs.stack.Copy()}
}

// ---------------------------------------------------------------------------------------------------------------------

func (tc *TypeChecker) equalTypeStacks(expected, actual *datatypes.TypeStack) bool {
	if expected.Size() != actual.Size() {
		return false
	}
	for i := range expected.Data {
		e := expected.Data[i]
		a := actual.Data[i]
		if e != datatypes.DataTypeAny && a != e {
			return false
		}
	}
	return true
}

func (tc *TypeChecker) enoughArgsCount(stack *datatypes.TypeStack, count int, token *tokens.Token) (err error) {
	if stack.Size() < count {
		err = logger.TypeCheckerError(&token.Loc, "Not enough arguments for `%s`", token.Text)
	}
	return
}

// ---------------------------------------------------------------------------------------------------------------------

func (tc *TypeChecker) popType(op *operations.Op, stack *datatypes.TypeStack, expected datatypes.DataType) error {
	if err := tc.enoughArgsCount(stack, 1, op.Token); err != nil {
		return err
	}

	actual := stack.Top()

	if expected != datatypes.DataTypeAny && actual != expected {
		return logger.TypeCheckerError(
			&op.Token.Loc, "Expected argument of type `%s` but got `%s`. Current stack: %s",
			datatypes.DataType2Str[expected], datatypes.DataType2Str[actual], stack.Data,
		)
	}
	stack.Pop()

	return nil
}

func (tc *TypeChecker) popTypes(op *operations.Op, stack, expected *datatypes.TypeStack) error {
	if err := tc.enoughArgsCount(stack, expected.Size(), op.Token); err != nil {
		return err
	}

	for i := expected.Size() - 1; i >= 0; i-- {
		t := expected.Data[i]
		if err := tc.popType(op, stack, t); err != nil {
			return err
		}
	}
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (tc *TypeChecker) popTypeContract(op *operations.Op, stack *datatypes.TypeStack, expected datatypes.DataType) (actual datatypes.DataType, err error) {
	actual = stack.Pop()

	if expected != datatypes.DataTypeAny && actual != expected {
		err = logger.TypeCheckerError(
			&op.Token.Loc, "Expected argument of type `%s` but got `%s`. Current stack: %s",
			datatypes.DataType2Str[expected], datatypes.DataType2Str[actual], stack.Data,
		)
	}
	return
}

func (tc *TypeChecker) popTypesContract(op *operations.Op, stack *datatypes.TypeStack, contract *Contract) (d datatypes.DataTypes, err error) {
	if err = tc.enoughArgsCount(stack, contract.Inputs.Size(), op.Token); err != nil {
		return
	}

	d = make(datatypes.DataTypes, contract.Inputs.Size())
	for i := contract.Inputs.Size() - 1; i >= 0; i-- {
		e := contract.Inputs.Data[i]
		d[i], err = tc.popTypeContract(op, stack, e)
		if err != nil {
			return
		}
	}
	return
}

// ---------------------------------------------------------------------------------------------------------------------

func (tc *TypeChecker) typeCheckOutputs(op *operations.Op, outputs datatypes.DataTypes, expected *datatypes.TypeStack) error {
	if len(outputs) != expected.Size() {
		return logger.TypeCheckerError(
			&op.Token.Loc, "Outputs for `%s` don't fit to its contract(expected: %s actual: %s)",
			op.Token.Text, expected.Data, outputs,
		)
	}

	for i, a := range outputs {
		e := expected.Data[i]
		if e != datatypes.DataTypeAny && e != a {
			return logger.TypeCheckerError(
				&op.Token.Loc, "Output arg #%d for `%s` don't fit to its contract(expected: %s actual: %s)",
				i, op.Token.Text, datatypes.DataType2Str[e], datatypes.DataType2Str[a],
			)
		}
	}
	return nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (tc *TypeChecker) typeCheckIntrinsic(op *operations.Op, i intrinsics.IntrinsicType, ctx *TypeCheckerContext) error {
	contract, err_msg := GetIntrinsicContract(i)
	if err_msg != "" {
		return logger.TypeCheckerError(&op.Token.Loc, "No contract for intrinsic found: %s", err_msg)
	}

	inputs, err := tc.popTypesContract(op, &ctx.Stack, contract)
	if err != nil {
		return err
	}

	logic, err_msg := GetIntrinsicLogic(i)
	if err_msg != "" {
		return logger.TypeCheckerError(&op.Token.Loc, "No logic for intrinsic found: %s", err_msg)
	}

	f := DefaultCustomFunc
	if i == intrinsics.IntrinsicTypeDebug {
		f = func() datatypes.DataTypes {
			fmt.Println(logger.FormatInfoMsg(&op.Token.Loc, "[TypeCheck DEBUG] stack: %s", ctx.Stack.Data))
			return datatypes.DataTypes{}
		}
	}

	outputs := logic(i, contract, inputs, f)
	if err := tc.typeCheckOutputs(op, outputs, contract.Outputs); err != nil {
		return err
	}

	for _, item := range outputs {
		ctx.Stack.Push(item)
	}
	return nil
}

func (tc *TypeChecker) typeCheckSimpleOps(op *operations.Op, ctx *TypeCheckerContext) error {
	contract, err_msg := GetSimpleOpContract(op.Typ)
	if err_msg != "" {
		return logger.TypeCheckerError(&op.Token.Loc, "No contract for simple operation found: %s", err_msg)
	}

	_, err := tc.popTypesContract(op, &ctx.Stack, contract)
	if err != nil {
		return err
	}

	for _, item := range contract.Outputs.Data {
		ctx.Stack.Push(item)
	}
	return nil
}

func (tc *TypeChecker) typeCheckFunc(ops *[]operations.Op, i int, contextStack *TypeCheckerContextStack) (index int, err error) {
	index = -1
	contextStackSize := contextStack.Size()

	ctx := contextStack.Top().Clone(context_type_func)
	contextStack.Push(ctx)

	func_name := (*ops)[i].Data.(string)
	loc := &(*ops)[i].Token.Loc

	f, exists := tc.Ctx.Funcs[func_name]
	if !exists {
		err = logger.TypeCheckerError(loc, "No such function: `%s`", func_name)
		return
	}
	for _, t := range f.Sig.Inputs.Data {
		ctx.Stack.Push(t)
	}

	if _, err = tc.typeCheck(ops, i+1, contextStack); err != nil {
		return
	}

	top := contextStack.Pop()
	if contextStack.Size() != contextStackSize {
		logger.TypeCheckerCrash(loc, "Function `%s` type checking does not preserve contextStack.Size()", func_name)
	}
	if top.Typ != context_type_func {
		logger.TypeCheckerCrash(
			loc, "The type of context has changed after function `%s` type checking. Expected %d but got %d",
			func_name, context_type_func, top.Typ,
		)
	}

	outputs := top.Outputs
	if !outputs.SameOutputs() {
		err = logger.TypeCheckerError(
			nil, "Function `%s` could finish with different output types:\n%s",
			func_name, outputs.FormatStacks(" * "),
		)
		return
	}

	final_stack := &outputs.Results[0].Stack
	if !tc.equalTypeStacks(&f.Sig.Outputs, final_stack) {
		err = logger.TypeCheckerError(
			loc, "Function `%s` does not fit to its signature. Expected `%s` but got `%s`",
			func_name, f.Sig.Outputs.Data, final_stack.Data,
		)
		return
	}
	index = outputs.Index + 1

	return
}

func (tc *TypeChecker) typeCheckWhileBlock(ops *[]operations.Op, i int, contextStack *TypeCheckerContextStack) (int, error) {
	index := -1
	contextStackSize := contextStack.Size()

	loc := &(*ops)[i].Token.Loc

	while_start := i + 1

	// process while case
	if _, err := tc.typeCheck(ops, while_start, contextStack); err != nil {
		return i, err
	}
	do_start := contextStack.Top().Outputs.Index

	ctx := contextStack.Top().Clone(context_type_while)
	contextStack.Push(ctx)

	if _, err := tc.typeCheck(ops, do_start+1, contextStack); err != nil {
		return i, err
	}
	end_index := contextStack.Top().Outputs.Index + 1
	if _, err := tc.typeCheck(ops, while_start, contextStack); err != nil {
		return i, err
	}

	top := contextStack.Pop()
	if contextStack.Size() != contextStackSize {
		logger.TypeCheckerCrash(loc, "While-loop type checking does not preserve contextStack.Size()")
	}
	if top.Typ != context_type_while {
		logger.TypeCheckerCrash(
			loc, "The type of context has changed after while-loop type checking. Expected %d but got %d",
			context_type_func, top.Typ,
		)
	}

	if top.Terminated {
		index = top.Outputs.Index
		top.Terminated = true
	} else {
		if !top.Outputs.SameOutputs() {
			return index, logger.TypeCheckerError(
				nil, "While-loop (%s) could finish with different stack results:\n%s",
				logger.FormatLoc(loc), top.Outputs.FormatStacks(" * "),
			)
		}

		if !utils.StacksAreEqual[datatypes.DataType](&contextStack.Top().Stack, &top.Outputs.Results[0].Stack) {
			return index, logger.TypeCheckerError(
				loc, "While-loop iteration changes stack. Expected `%s` but got `%s`",
				contextStack.Top().Stack.Data, top.Stack.Data,
			)
		}
		index = end_index
	}
	return index, nil
}

/*
Example of how the spellchecker checks if-elif-else-end blocks:

if[0] () [1]do
()
[2]elif[3] () [4]do
()
[5]elif[6] () [7]do
()
[8]else[9]
()
[10]end

* 0-1-(copy stack)-2
* 0-1-(copy stack)-3-4-(copy stack)-5
* 0-1-(copy stack)-3-4-(copy stack)-6-7-(copy stack)-8
* 0-1-(copy stack)-3-4-(copy stack)-6-7-(copy stack)-9-10
*/
func (tc *TypeChecker) typeCheckIfBlock(ops *[]operations.Op, i int, contextStack *TypeCheckerContextStack) (int, error) {
	index := -1
	loc := &(*ops)[i].Token.Loc

	var true_stack, false_stack, current_stack *TypeCheckerContextStack
	results := make([]*TypeCheckerContext, 0)
	current_stack = contextStack

	onlyif := true
loop:
	for {
		if _, err := tc.typeCheck(ops, i+1, current_stack); err != nil {
			return i, err
		}
		top := current_stack.Top()
		i = top.Outputs.Index

		do_op_operand := int((*ops)[i].Operand.(types.IntType))

		j := i + do_op_operand - 1 // j points to `elif`, `else` or `end`

		true_stack = current_stack.Clone()
		false_stack = current_stack.Clone()
		true_stack.Push(top.Clone(context_type_if))

		switch (*ops)[j].Data.(operations.OpJumpType) {
		case operations.OpJumpElif: // .. do (..) elif ..
			ti, err := tc.typeCheck(ops, i+1, true_stack)
			if err != nil {
				return index, err
			}
			i = j
			results = append(results, true_stack.Top())
			if !true_stack.Top().Terminated {
				index = ti + 1
			}
			current_stack = false_stack
			onlyif = false
		case operations.OpJumpElse: // .. do (..) else (..) end ...
			if _, err := tc.typeCheck(ops, i+1, true_stack); err != nil {
				return index, err
			}
			results = append(results, true_stack.Top())

			false_stack.Push(top.Clone(context_type_if))
			if _, err := tc.typeCheck(ops, j+1, false_stack); err != nil {
				return index, err
			}
			results = append(results, false_stack.Top())

			index = j + int((*ops)[j].Operand.(types.IntType))
			break loop
		case operations.OpJumpEnd: // ... do (..) end ...
			if onlyif {
				falsed := current_stack.Top().Clone(context_type_if)
				falsed.Outputs.Index = j
				results = append(results, falsed)
			}
			if _, err := tc.typeCheck(ops, i+1, true_stack); err != nil {
				return index, err
			}
			results = append(results, true_stack.Top())
			index = j + 1

			break loop
		default:
			return index, logger.TypeCheckerError(loc, "UNHANDLED OpJumpType")
		}
	}

	terminated := make([]*TypeCheckerContext, 0)
	not_terminated := make([]*TypeCheckerContext, 0)
	for _, c := range results {
		if c.Terminated {
			terminated = append(terminated, c)
		} else {
			not_terminated = append(not_terminated, c)
		}
	}

	switch len(not_terminated) {
	case 0:
		if len(terminated) == 0 {
			logger.TypeCheckerCrash(loc, "Both terminated and not_terminated contexts are empty")
		}
		index = terminated[0].Outputs.Index // go to termination (`end` for while-loops or functions)
		contextStack.Top().Stack = terminated[0].Stack
		contextStack.Top().Terminated = true
	default:
		same := true
		for k := 0; k < len(not_terminated)-1; k++ {
			same = same && ContextsAreSame(not_terminated[k], not_terminated[k+1])
		}
		if !same {
			return index, logger.TypeCheckerError(
				nil, "not_terminated contexts for if-block (%s) are not the same:\n%s",
				logger.FormatLoc(loc), FormatContextSliceOutputs(not_terminated, " * "),
			)
		}
		contextStack.Top().Stack = not_terminated[0].Stack
	}
	return index, nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (tc *TypeChecker) typeCheck(ops *[]operations.Op, start int, contextStack *TypeCheckerContextStack) (i int, err error) {
	ctx := contextStack.Top()

	for i = start; i < len(*ops); {
		op := &(*ops)[i]
		switch op.Typ {
		case operations.OpPushInt, operations.OpPushBool, operations.OpPushPtr, operations.OpPushFptr, operations.OpPushGlobalAlloc, operations.OpPushLocalAlloc:
			if err = tc.typeCheckSimpleOps(op, ctx); err != nil {
				return
			}
			i++
		case operations.OpIntrinsic:
			if err = tc.typeCheckIntrinsic(op, op.Operand.(intrinsics.IntrinsicType), ctx); err != nil {
				return
			}
			i++

		case operations.OpJump:
			// process if-else-end, while-break-continue-end and return differently
			block_type := op.Data.(operations.OpJumpType)
			switch block_type {
			case operations.OpJumpReturn:
				func_ctx := contextStack.GetContext(context_type_func)
				func_ctx.Outputs.Results = append(func_ctx.Outputs.Results, TypeCheckerJumpResult{
					Stack: *ctx.Stack.Copy(), Token: op.Token,
				})
				func_ctx.Outputs.Index = i + int(op.Operand.(types.IntType))

				if ctx.Typ == context_type_if || ctx.Typ == context_type_while {
					ctx.Terminated = true
					ctx.Outputs.Index = func_ctx.Outputs.Index
				}
				return i, nil

			case operations.OpJumpEnd:
				ctx.Outputs.Results = append(ctx.Outputs.Results, TypeCheckerJumpResult{
					Stack: *ctx.Stack.Copy(), Token: op.Token,
				})
				ctx.Outputs.Index = i
				return i, nil
			case operations.OpJumpBreak, operations.OpJumpContinue:
				while_ctx := contextStack.GetContext(context_type_while)
				while_ctx.Outputs.Results = append(while_ctx.Outputs.Results, TypeCheckerJumpResult{
					Stack: *ctx.Stack.Copy(), Token: op.Token,
				})

				while_ctx.Outputs.Index = i + int(op.Operand.(types.IntType))
				if block_type == operations.OpJumpBreak {
					while_ctx.Outputs.Index -= 1
				}

				if ctx.Typ == context_type_if {
					ctx.Terminated = true
					ctx.Outputs.Index = while_ctx.Outputs.Index
				}
				return i, nil

			case operations.OpJumpWhile:
				i, err = tc.typeCheckWhileBlock(ops, i, contextStack)
				if err != nil {
					return
				}

			case operations.OpJumpIf:
				i, err = tc.typeCheckIfBlock(ops, i, contextStack)
				if err != nil {
					return
				}
			case operations.OpJumpElse:
				ctx.Outputs.Results = append(ctx.Outputs.Results, TypeCheckerJumpResult{
					Stack: *ctx.Stack.Copy(), Token: op.Token,
				})
				ctx.Outputs.Index = i + int(op.Operand.(types.IntType)) - 1
				return i, nil
			case operations.OpJumpElif:
				ctx.Outputs.Results = append(ctx.Outputs.Results, TypeCheckerJumpResult{
					Stack: *ctx.Stack.Copy(), Token: op.Token,
				})
				ctx.Outputs.Index = i
				return i, nil
			default:
				return i, logger.TypeCheckerError(&op.Token.Loc, "Type checking for %s (OpJump) is not implemented yet", operations.OpJumpType2Str[block_type])
			}

		case operations.OpCondJump:
			if err = tc.popType(op, &ctx.Stack, datatypes.DataTypeBool); err != nil {
				return
			}
			ctx.Outputs.Index = i
			return

		case operations.OpFuncBegin:
			i, err = tc.typeCheckFunc(ops, i, contextStack)
			if err != nil {
				return
			}
		case operations.OpFuncEnd:
			ctx.Outputs.Results = append(ctx.Outputs.Results, TypeCheckerJumpResult{
				Stack: *ctx.Stack.Copy(), Token: op.Token,
			})
			ctx.Outputs.Index = i
			return i, nil

		case operations.OpCall:
			f := tc.Ctx.Funcs[op.Data.(string)]
			if err = tc.popTypes(op, &ctx.Stack, &f.Sig.Inputs); err != nil {
				return
			}
			for _, t := range f.Sig.Outputs.Data {
				ctx.Stack.Push(t)
			}
			i++

		case operations.OpCapture:
			typs := op.Data.(datatypes.DataTypes)
			if ctx.Stack.Size() < len(typs) {
				err = logger.TypeCheckerError(&op.Token.Loc, "Not enought variables to capture: expected %s but got %s", typs, ctx.Stack.Data)
				return
			}

			for j := 0; j < len(typs); j++ {
				e := ctx.Stack.Data[ctx.Stack.Size()-len(typs)+j]
				a := typs[j]
				if e != datatypes.DataTypeAny && e != a {
					err = logger.TypeCheckerError(&op.Token.Loc, "Capture types mismatch: expected %s but got %s", typs, ctx.Stack.Data)
					return
				}
				ctx.Captures.Push(a)
			}
			i++
		case operations.OpDropCaptures:
			drop_count := op.Operand.(types.IntType)
			if ctx.Captures.Size() < int(drop_count) {
				err = logger.TypeCheckerError(&op.Token.Loc, "Not enought variables in capture stack to drop: expected %d but got %d", ctx.Captures.Size(), drop_count)
				return
			}
			for j := 0; j < int(drop_count); j++ {
				ctx.Captures.Pop()
			}
			i++
		case operations.OpPushCaptured:
			idx := op.Operand.(types.IntType)

			if idx >= types.IntType(ctx.Captures.Size()) {
				err = logger.TypeCheckerError(&op.Token.Loc, "Not enough variables in capture stack to push: expected to push %d but got %d", idx, ctx.Captures.Size())
				return i, err
			}
			t := ctx.Captures.Data[types.IntType(ctx.Captures.Size())-1-idx]
			ctx.Stack.Push(t)
			i++
		case operations.OpCallLike:
			if err = tc.popType(op, &ctx.Stack, datatypes.DataTypeFptr); err != nil {
				return
			}

			f := tc.Ctx.Funcs[op.Data.(string)]
			if err = tc.popTypes(op, &ctx.Stack, &f.Sig.Inputs); err != nil {
				return
			}
			for _, t := range f.Sig.Outputs.Data {
				ctx.Stack.Push(t)
			}
			i++

		default:
			err = logger.TypeCheckerError(&op.Token.Loc, "Type check for `%s` op is not implemented yet", operations.OpType2Str[op.Typ])
			return
		}
	}

	top := contextStack.Top()
	if top.Typ != context_type_global || contextStack.Size() != 1 {
		logger.TypeCheckerCrash(nil, "Out of ops but got non-empty contextStack or non-global context.Typ")
	}
	top.Outputs.Results = append(top.Outputs.Results, TypeCheckerJumpResult{
		Stack: *ctx.Stack.Copy(), Token: nil,
	})
	top.Outputs.Index = len(*ops)

	return i, nil
}

// ---------------------------------------------------------------------------------------------------------------------

func (tc *TypeChecker) TypeCheckProgram(ops *[]operations.Op, ctx *compiler.CompileTimeContext, s *settings.Settings) error {
	defer logger.Timeit(logger.ModuleTypeChecker, s.LogLevel)()

	if !tc.enabled {
		return nil
	}

	tc.Ctx = ctx

	contextStack := NewTypeCheckerContextStack()
	context := NewTypeCheckerContext(context_type_global)
	contextStack.Push(context)

	_, err := tc.typeCheck(ops, 0, contextStack)
	if err != nil {
		return err
	}

	context = contextStack.Pop()
	if context.Outputs.Index != len(*ops) {
		return logger.TypeCheckerError(nil, "Not all ops were typechecked (have %d in total, but end=%d)", len(*ops), context.Outputs.Index)
	}
	if len(context.Outputs.Results) != 1 {
		return logger.TypeCheckerError(nil, "There should be only one stack in TypeCheckerOutputs but got %d", len(context.Outputs.Results))
	}

	stack := context.Outputs.Results[0].Stack
	if !(context.Typ == context_type_global && stack.Size() == 1 && stack.Top() == datatypes.DataTypeInt) {
		return logger.TypeCheckerError(nil, "Expected typecheck stack to contain `int` as exit code at the end, but got %s", stack.Data)
	}
	return nil
}
