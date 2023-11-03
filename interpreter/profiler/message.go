package profiler

import (
	"Gorth/interpreter/types"
	"fmt"
	"sort"

	"golang.org/x/exp/constraints"
)

type Message struct {
	OpId types.IntType
	Time int64
}

type OpTimings struct {
	Timing []int64
}

func NewOpTimings() *OpTimings {
	return &OpTimings{
		Timing: make([]int64, 0),
	}
}

type OpStat struct {
	Id     types.IntType
	Count  int
	Total  int64
	Mean   float32
	Median float32
	Max    int64
	Min    int64
}

func NewOpStat(id types.IntType, t *OpTimings) OpStat {
	s := OpStat{Id: id, Count: len(t.Timing)}

	s.Total = Sum(&t.Timing)
	s.Mean = float32(s.Total) / float32(s.Count)
	s.Max = Max(&t.Timing)
	s.Min = Min(&t.Timing)

	sort.Slice(t.Timing, func(i, j int) bool { return t.Timing[i] < t.Timing[j] })
	if len(t.Timing)%2 == 0 {
		s.Median = float32(t.Timing[len(t.Timing)/2]+t.Timing[len(t.Timing)/2-1]) / 2
	} else {
		s.Median = float32(t.Timing[len(t.Timing)/2])
	}

	return s
}

func PrintHeader() {
	fmt.Printf(
		"%5s\t%8s\t%9s\t%8s\t%8s\t%8s\t%10s\n",
		"id", "count", "total[ns]", "mean[ns]", "min[ns]", "max[ns]", "median[ns]",
	)
}

func (o *OpStat) Print() {
	fmt.Printf(
		"%5d\t%8d\t%9d\t%8.2f\t%8d\t%8d\t%10.1f\n",
		o.Id, o.Count, o.Total, o.Mean, o.Min, o.Max, o.Median,
	)
}

type Number interface {
	constraints.Integer | constraints.Float
}

func Reduce[T Number](f func(T, T) T, nums *[]T, init T) T {
	acc := init
	for _, x := range *nums {
		acc = f(acc, x)
	}
	return acc
}

func Sum[T Number](nums *[]T) T {
	return Reduce(func(x, y T) T { return x + y }, nums, 0)
}

func Max[T Number](nums *[]T) T {
	return Reduce(func(x, y T) T {
		if x > y {
			return x
		} else {
			return y
		}
	}, nums, (*nums)[0])
}

func Min[T Number](nums *[]T) T {
	return Reduce(func(x, y T) T {
		if x < y {
			return x
		} else {
			return y
		}
	}, nums, (*nums)[0])
}
