package optimizer

import (
	"Gorth/interpreter/logger"
)

type OptimizeLevel = int

const (
	OptLevelNo   OptimizeLevel = iota // no optimizations
	OptLevelBase                      // eliminate dead code
)

func OptLevelFromInt(n int) OptimizeLevel {
	switch n {
	case 0:
		return OptLevelNo
	case 1:
		return OptLevelBase
	default:
		logger.OptimizerCrash(nil, "Unhanded value for OptimizeLevel: %d", n)
		return OptLevelNo
	}
}
