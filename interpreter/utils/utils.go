package utils

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"reflect"
	"strings"
)

func Exit(exitcode int) {
	os.Exit(exitcode)
}

func RevMap(i interface{}) interface{} {
	// Get type
	t := reflect.TypeOf(i)

	switch t.Kind() {
	case reflect.Map:
		// Get the value of the provided map
		v := reflect.ValueOf(i)

		// Create the map of the specific type. Key type is t.Key(), and element type is it
		m := reflect.MakeMap(reflect.MapOf(t.Elem(), t.Key()))

		// Copy values to new map
		for _, mk := range v.MapKeys() {
			m.SetMapIndex(v.MapIndex(mk), mk)
		}

		return m.Interface()
	}
	panic(fmt.Sprintf("Unsupported type of input argument in utils.RevMap: %#v", t))
}

func ReadFile(fn string) (lines []string) {
	file, err := os.Open(fn)
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		if err = file.Close(); err != nil {
			log.Fatal(err)
		}
	}()

	reader := bufio.NewReader(file)
	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				break
			}

			log.Fatalf("read file line error: %v", err)
			return
		}
		line = strings.TrimRight(line, "\n")
		lines = append(lines, line)
	}

	return
}
