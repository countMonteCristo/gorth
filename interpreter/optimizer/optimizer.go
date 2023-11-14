package optimizer

import (
	"Gorth/interpreter/compiler"
	"Gorth/interpreter/logger"
	"Gorth/interpreter/settings"
	"Gorth/interpreter/types"
	"Gorth/interpreter/vm"
)

type Optimizer struct {
	ctx   *compiler.CompileTimeContext
	level OptimizeLevel
}

func NewOptimizer(level int) *Optimizer {
	return &Optimizer{level: OptLevelFromInt(level)}
}

func (o *Optimizer) scanFunction(ops *[]vm.Op, func_name string, used *map[string]bool) error {
	if _, exists := (*used)[func_name]; exists {
		return nil
	}

	f, exists := o.ctx.Funcs[func_name]
	if !exists {
		return logger.OptimizerError(nil, "Unknown function in optimizer: %s", func_name)
	}
	(*used)[func_name] = true

loop:
	for i := f.Addr; i < types.IntType(len(*ops)); i++ {
		op := (*ops)[i]
		switch op.Typ {
		case vm.OpCall, vm.OpPushFptr:
			call_name := op.Data.(string)
			o.scanFunction(ops, call_name, used)
		case vm.OpFuncEnd:
			break loop
		}
	}
	return nil
}

func (o *Optimizer) collectUnusedFunctions(ops *[]vm.Op) (map[string]bool, error) {
	used_functions := make(map[string]bool, 0)
	err := o.scanFunction(ops, compiler.EntryPointName, &used_functions)
	return used_functions, err
}

func (o *Optimizer) eliminateDeadCode(ops, optimized *[]vm.Op) error {
	used_functions, err := o.collectUnusedFunctions(ops)
	if err != nil {
		return err
	}
	new_addresses := make(map[string]types.IntType, 0)

	use := true
	for _, op := range *ops {
		switch op.Typ {
		case vm.OpFuncBegin:
			func_name := op.Data.(string)
			use = used_functions[func_name]
			if use {
				new_addresses[func_name] = types.IntType(len(*optimized))
			}
		case vm.OpCall:
			if use {
				func_name := op.Data.(string)
				op.Operand = new_addresses[func_name] - types.IntType(len(*optimized))
			}
		case vm.OpPushFptr:
			if use {
				func_name := op.Data.(string)
				op.Operand = new_addresses[func_name]
			}
		}
		if use {
			*optimized = append(*optimized, op)
		}
	}
	return nil
}

func (o *Optimizer) Optimize(ops *[]vm.Op, ctx *compiler.CompileTimeContext, rts *vm.RuntimeSettings, s *settings.Settings) (err error) {
	defer logger.Timeit(logger.ModuleOptimizer, s.LogLevel)()

	o.ctx = ctx

	if o.level == OptLevelNo {
		return
	}

	optimized := make([]vm.Op, 0)

	if o.level == OptLevelBase {
		if err = o.eliminateDeadCode(ops, &optimized); err != nil {
			return
		}
	}

	*ops = optimized

	rts.OpsCount = types.IntType(len(optimized))
	rts.EntryPointAddr = rts.OpsCount - 1

	return
}
