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

	var outbuf strings.Builder
	// var errbuf strings.Builder
	cmd.Stdout = &outbuf
	// cmd.Stderr = &errbuf

	err := cmd.Run()
	stdout := outbuf.String()
	// stderr := errbuf.String()

	if err != nil {
		fmt.Println(stdout)
		return
	}

	actual_output := strings.Split(strings.Trim(stdout, "\n"), "\n")

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
		// fmt.Printf("Append line `%s`\n", line)
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

func ListDir(dir string) (fns []string) {
	files, err := ioutil.ReadDir(dir)
	if err != nil {
		log.Fatal(err)
	}

	for _, f := range files {
		ext := filepath.Ext(f.Name())
		if ext != ".gorth" {
			continue
		} else {
			full_name := filepath.Join(dir, f.Name())
			fns = append(fns, full_name)
		}
	}
	return
}

func TestFile(fn string, stats *Stats) {

	ext := filepath.Ext(fn)
	name := fn[:len(fn)-len(ext)]
	expected_output_file := name + ".txt"
	expected_output := LoadExpected(expected_output_file)

	testcase := TestCase{
		File: fn,
		Cmd: []string{
			"go", "run", "gorth.go", fn,
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
		panic(fmt.Sprintf("Unhandled testcase status in TestFile(): %d\n", status))
	}
}

func TestInputs(gorth_fns []string) {
	stats := Stats{}

	for _, gorth_fn := range gorth_fns {
		TestFile(gorth_fn, &stats)
	}

	fmt.Println()
	fmt.Printf("Total tests: %d:\n", stats.Total)
	fmt.Printf("  succeeded: %d\n", stats.Success)
	fmt.Printf("  failed:    %d\n", stats.Fail)
	fmt.Printf("  skiped:    %d\n", stats.Skip)
}

func PrepareInputs(in_paths []string) (gorth_fns []string) {
	for _, path := range in_paths {

		fin, err := os.Open(path)
		if err != nil {
			fmt.Printf("WARNING: File or directory `%s` does not exist\n", path)
			continue
		}

		fstat, err := fin.Stat()
		if err != nil {
			fmt.Printf("WARNING: Can not get stat for file or directory: `%s`\n", path)
			continue
		}

		switch {
		case fstat.IsDir():
			dir_gorth_fns := ListDir(path)
			gorth_fns = append(gorth_fns, dir_gorth_fns...)
		default:
			ext := filepath.Ext(path)
			if ext != ".gorth" {
				continue
			} else {
				gorth_fns = append(gorth_fns, path)
			}
		}
	}
	return
}

func main() {
	input_paths := os.Args[1:]
	fmt.Printf("Run tests from %v\n", input_paths)
	TestInputs(PrepareInputs(input_paths))
}
