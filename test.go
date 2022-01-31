package main

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

type Stats struct {
	Total   int
	Success int
	Fail    int
	Skip    int
}

const (
	StatusSuccess = iota
	StatusFail    = iota
	StatusSkip    = iota
)

type TestCase struct {
	File           string
	Cmd            []string
	ExpectedOutput []string
}

func (t *TestCase) run() (status int) {
	fmt.Printf("Running testcase %s...\n", t.File)
	status = StatusFail

	cmd := exec.Command(t.Cmd[0], t.Cmd[1:]...)

	stdout, err := cmd.CombinedOutput()
	if err != nil {
		fmt.Println(err.Error())
		return
	}

	actual_output := strings.Split(strings.Trim(string(stdout), "\n"), "\n")

	ok := ChekOutput(actual_output, t.ExpectedOutput)
	if ok {
		fmt.Printf("SUCCESS\n")
		status = StatusSuccess
	} else {
		fmt.Printf("FAILED\n")
		status = StatusFail
	}

	return
}

func LoadExpected(fn string) (lines []string) {
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
		// fmt.Printf("Append line <%s>\n", line)
	}

	return
}

func ChekOutput(actual, expected []string) (result bool) {
	result = false
	if len(actual) != len(expected) {
		fmt.Printf("INFO: Test failed!\n")
		fmt.Printf("\tActual:   %v\n", actual)
		fmt.Printf("\tExpected: %v\n", expected)
		return
	}

	for idx, line := range actual {
		if line != expected[idx] {
			fmt.Printf("INFO: Test failed (actual[%d]=`%s` while expected[%d]=`%s`)\n", idx, actual[idx], idx, expected[idx])
			fmt.Printf("\tActual:   %v\n", actual)
			fmt.Printf("\tExpected: %v\n", expected)
			return
		}
	}
	result = true
	return
}

func TestFolder(folder string) {
	files, err := ioutil.ReadDir(folder)
	if err != nil {
		log.Fatal(err)
	}

	stats := Stats{}
	for _, f := range files {
		ext := filepath.Ext(f.Name())
		if ext != ".gorth" {
			continue
		}

		stats.Total++
		full_name := filepath.Join(folder, f.Name())

		name := f.Name()[:len(f.Name())-len(ext)]
		expected_output_file := name + ".txt"
		expected_output := LoadExpected(filepath.Join(folder, expected_output_file))

		testcase := TestCase{
			File: full_name,
			Cmd: []string{
				"go", "run", "gorth.go", full_name,
			},
			ExpectedOutput: expected_output,
		}

		status := testcase.run()
		switch status {
		case StatusSuccess:
			stats.Success++
		case StatusFail:
			stats.Fail++
		case StatusSkip:
			stats.Skip++
		default:
			panic(fmt.Sprintf("Unhandled testcase status in TestFolder(): %d\n", status))
		}
	}

	fmt.Println()
	fmt.Printf("Total tests: %d:\n", stats.Total)
	fmt.Printf("  succeeded: %d\n", stats.Success)
	fmt.Printf("  failed:    %d\n", stats.Fail)
	fmt.Printf("  skiped:    %d\n", stats.Skip)
}

func main() {
	folder := os.Args[1]
	fmt.Printf("Run tests from %s\n", folder)
	TestFolder(folder)
}
