package profiler

import (
	"Gorth/interpreter/types"
	"Gorth/interpreter/vm"
	"fmt"
	"time"
)

type Profiler struct {
	logger Logger
	vm     *vm.VM
}

func NewProfiler(vm *vm.VM) *Profiler {
	return &Profiler{
		logger: *NewLogger(), vm: vm,
	}
}

func (p *Profiler) Run(ops *[]vm.Op, args []string) {
	go p.logger.Run()

	var err error = nil
	for p.vm.Rc.Addr < p.vm.Rc.OpsCount {
		start := time.Now().UnixNano()
		m := Message{OpId: p.vm.Rc.Addr, Time: 0}
		err = p.vm.Step(ops)
		m.Time = time.Now().UnixNano() - start
		p.logger.Messages <- m
		if err != nil {
			break
		}
	}
	p.logger.Messages <- Message{OpId: -1}
	close(p.logger.Messages)

	p.Summurize()
}

func (p *Profiler) Summurize() {
	fmt.Printf("\n\n========================================== PROFILE STATS ===========================================\n")
	PrintHeader()
	for id := types.IntType(0); id < p.vm.Rc.OpsCount; id++ {
		if timings, exists := p.logger.Stats[id]; exists {
			stats := NewOpStat(id, timings)
			stats.Print()
		}
	}
	fmt.Printf("====================================================================================================\n")
}
