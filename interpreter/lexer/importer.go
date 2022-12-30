package lexer

import (
	"log"
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
	err := filepath.Walk(".",
		func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if strings.HasSuffix(path, fn) {
				results = append(results, path)
			}
			return nil
		})
	if err != nil {
		// TODO: proper error message
		log.Fatal(err)
	}

	if len(results) > 0 {
		return results[0], true
	}
	return fn, false
}
