package profiler

import (
	"Gorth/interpreter/logger"
	"Gorth/interpreter/operations"
	"Gorth/interpreter/settings"
	"Gorth/interpreter/types"
	"Gorth/interpreter/vm"
	"fmt"
	"os"
	"strings"
	"time"
)

type Profiler struct {
	logger Logger
	vm     *vm.VM
	s      *settings.Settings
	fn     string
}

func NewProfiler(vm *vm.VM, fn string, s *settings.Settings) *Profiler {
	return &Profiler{
		logger: *NewLogger(), vm: vm, fn: fn, s: s,
	}
}

func (p *Profiler) Run(ops *[]operations.Op, args []string) {
	go p.logger.Run()

	var err error = nil
	for p.vm.Rc.Addr < p.vm.Rc.OpsCount {
		start := time.Now().UnixNano()
		m := Message{OpId: p.vm.Rc.Addr, Time: 0}
		err = p.vm.Step(ops, p.s)
		m.Time = time.Now().UnixNano() - start
		p.logger.Messages <- m
		if err != nil {
			break
		}
	}
	p.logger.Messages <- Message{OpId: -1}
	close(p.logger.Messages)

	p.Summurize(ops)
}

func (p *Profiler) Summurize(ops *[]operations.Op) {
	fout, err := os.Create(p.fn)
	if err != nil {
		logger.ProfilerCrash(nil, "Cannot create profiler file %s: %s", p.fn, err)
	}
	defer func() {
		if err := fout.Close(); err != nil {
			logger.ProfilerCrash(nil, "Cannot close profiler file %s: %s", p.fn, err)
		}
	}()

	sep := fmt.Sprintf("%s PROFILE STATS %s", strings.Repeat("=", 60), strings.Repeat("=", 60))

	fmt.Fprintf(fout, "%s\n", sep)
	PrintHeader(fout)
	for id := types.IntType(0); id < p.vm.Rc.OpsCount; id++ {
		if timings, exists := p.logger.Stats[id]; exists {
			stats := NewOpStat(id, timings)
			stats.Print(fout, &(*ops)[id])
		}
	}
	fmt.Fprintf(fout, "%s\n", strings.Repeat("=", len(sep)))
}
