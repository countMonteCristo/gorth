package logger

import (
	"fmt"
	"os"
	"time"
)

func Timeit(mod ModuleType, global_level LogLevelType) func() {
	start := time.Now().UnixNano()
	return func() {
		if global_level <= Debug {
			fmt.Fprint(
				os.Stderr,
				FormatMsg(
					mod, Debug,
					"Time elapsed: %f ms\n",
					float64(time.Now().UnixNano()-start)/1000000.0),
			)
		}
	}
}
