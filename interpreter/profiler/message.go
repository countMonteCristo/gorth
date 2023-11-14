package profiler

import (
	"Gorth/interpreter/operations"
	"Gorth/interpreter/types"
	"fmt"
	"io"
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

func PrintHeader(w io.Writer) {
	fmt.Fprintf(w,
		"%5s\t%12s\t%8s\t%9s\t%8s\t%8s\t%8s\t%10s\t%s\n",
		"id", "op", "count", "total[ns]", "mean[ns]", "min[ns]", "max[ns]", "median[ns]", "loc",
	)
}

func (o *OpStat) Print(w io.Writer, op *operations.Op) {
	fmt.Fprintf(w,
		"%5d\t%12s\t%8d\t%9d\t%8.2f\t%8d\t%8d\t%10.1f\t%s\n",
		o.Id, operations.OpType2Str[op.Typ], o.Count, o.Total, o.Mean, o.Min, o.Max, o.Median, &op.Token.Loc,
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
