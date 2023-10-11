package lexer

import (
	"Gorth/interpreter/logger"
	"Gorth/interpreter/utils"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

type Importer struct {
	Paths    []string        // where to find imported files
	Included map[string]bool // already included files
}

func NewImporter(pkg_dir string, include_paths utils.ArrayArgs) *Importer {
	i := &Importer{Paths: make([]string, 0), Included: make(map[string]bool)}
	i.Paths = append(i.Paths, pkg_dir)
	i.Paths = append(i.Paths, include_paths...)
	return i
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
			logger.LexerCrash(nil, fmt.Sprintf("Import error: %s", err.Error()))
		}

		if len(results) > 0 {
			return results[0], true
		}
	}
	return fn, false
}
