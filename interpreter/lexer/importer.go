package lexer

import (
	"Gorth/interpreter/logger"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

type Importer struct {
	Paths    []string        // where to find imported files
	Included map[string]bool // already included files
}

func (i *Importer) Find(fn string) (string, bool) {
	results := make([]string, 0)

	for _, include_path := range i.Paths {
		err := filepath.Walk(include_path,
			func(path string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if strings.HasSuffix(path, fn) && len(path) > len(include_path)+len(fn) {
					results = append(results, path)
				}
				return nil
			})
		if err != nil {
			logger.Crash(nil, fmt.Sprintf("Import error: %s", err.Error()))
		}

		if len(results) > 0 {
			return results[0], true
		}
	}
	return fn, false
}
