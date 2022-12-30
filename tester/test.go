package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

type Stats struct {
	Total    int
	Detailed map[int]int
}

const (
	StatusSuccess = iota
	StatusFail
	StatusSkip
)

type TestConfig struct {
	Argv     []string `json:"argv"`
	Stdin    string   `json:"stdin"`
	Stdout   string   `json:"stdout"`
	Stderr   string   `json:"stderr"`
	ExitCode int      `json:"exit_code"`
}

type TestOutput struct {
	Stdout, Stderr string
	ExitCode       int
}

type TestCase struct {
	File   string
	Cmd    []string
	Config TestConfig
}

func NewTestCase(fn string) TestCase {
	return TestCase{
		File: fn,
		Config: TestConfig{
			Argv: make([]string, 0), Stdin: "", Stdout: "", Stderr: "", ExitCode: 0,
		},
		Cmd: []string{
			"go", "run", "gorth.go", fn,
		},
	}
}

func (t *TestCase) GetExpectedFilePath() string {
	ext := filepath.Ext(t.File)
	name := t.File[:len(t.File)-len(ext)]
	return name + ".txt"
}

func (t *TestCase) run() TestOutput {
	fmt.Printf("Running testcase %s\n", t.File)

	cmd := exec.Command(t.Cmd[0], t.Cmd[1:]...)

	stdin, err := cmd.StdinPipe()
	if err != nil {
		log.Fatal(err)
	}
	defer stdin.Close()

	io.WriteString(stdin, t.Config.Stdin)

	var outbuf, errbuf strings.Builder
	cmd.Stdout, cmd.Stderr = &outbuf, &errbuf

	if err = cmd.Start(); err != nil {
		fmt.Fprintf(os.Stderr, "An error occured: %s", err) //replace with logger, or anything you want
	}
	cmd.Wait()

	return TestOutput{
		Stdout: outbuf.String(), Stderr: errbuf.String(), ExitCode: cmd.ProcessState.ExitCode(),
	}
}

func (t *TestCase) LoadExpected(fn string) (exists bool) {
	if _, err := os.Stat(fn); errors.Is(err, os.ErrNotExist) {
		return false
	}
	exists = true

	file, err := os.Open(fn)
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		if err = file.Close(); err != nil {
			log.Fatal(err)
		}
	}()

	decoder := json.NewDecoder(file)
	err = decoder.Decode(&t.Config)
	if err != nil {
		log.Fatal(err)
	}
	t.Cmd = append(t.Cmd, t.Config.Argv...)
	return
}

func (t *TestCase) Check(output TestOutput) (status int) {
	status = StatusFail

	stdout_ok := CheckOutput(output.Stdout, t.Config.Stdout, "STDOUT")
	stderr_ok := CheckOutput(output.Stderr, t.Config.Stderr, "STDERR")
	exit_code_ok := (output.ExitCode == t.Config.ExitCode)

	if !exit_code_ok {
		fmt.Fprintln(os.Stderr, "  EXITCODE differs:")
		fmt.Fprintf(os.Stderr, "\tActual:   %d\n", output.ExitCode)
		fmt.Fprintf(os.Stderr, "\tExpected: %d\n", t.Config.ExitCode)
	}

	if stdout_ok && stderr_ok && exit_code_ok {
		fmt.Fprintln(os.Stderr, "SUCCESS")
		status = StatusSuccess
	} else {
		fmt.Fprintln(os.Stderr, "FAILED")
		status = StatusFail
	}
	return
}

func CheckOutput(actual, expected, out_desc string) bool {
	if actual != expected {
		fmt.Fprintf(os.Stderr, "  %s differs:\n", out_desc)
		fmt.Fprintf(os.Stderr, "\tActual:   `%v`\n", actual)
		fmt.Fprintf(os.Stderr, "\tExpected: `%v`\n", expected)
		return false
	}

	return true
}

func ListDir(dir string) (fns []string) {
	files, err := os.ReadDir(dir)
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
	testcase := NewTestCase(fn)
	expected_output_file := testcase.GetExpectedFilePath()

	var status int
	exists := testcase.LoadExpected(expected_output_file)
	if !exists {
		status = StatusSkip
		fmt.Fprintf(os.Stderr, "Running testcase %s\n", fn)
		fmt.Fprintf(os.Stderr, "  Config file %s not found, skip testcase\n", expected_output_file)
		fmt.Println("SKIP")
	} else {
		output := testcase.run()
		status = testcase.Check(output)
	}

	stats.Total++
	stats.Detailed[status]++
}

func TestInputs(gorth_fns []string) {
	stats := Stats{Detailed: map[int]int{StatusSuccess: 0, StatusSkip: 0, StatusFail: 0}}

	for _, gorth_fn := range gorth_fns {
		TestFile(gorth_fn, &stats)
	}

	fmt.Println()
	fmt.Printf("Total tests: %d:\n", stats.Total)
	fmt.Printf("  succeeded: %d\n", stats.Detailed[StatusSuccess])
	fmt.Printf("  failed:    %d\n", stats.Detailed[StatusFail])
	fmt.Printf("  skiped:    %d\n", stats.Detailed[StatusSkip])
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

func RecordTestOutputs(gorth_fns []string) {
	for _, gorth_fn := range gorth_fns {
		RecordOutput(gorth_fn)
	}
}

func RecordOutput(fn string) {
	testcase := NewTestCase(fn)
	expected_output_file := testcase.GetExpectedFilePath()

	testcase.LoadExpected(expected_output_file)
	output := testcase.run()

	new_config := TestConfig{
		Argv: testcase.Config.Argv, Stdin: testcase.Config.Stdin,
		Stdout:   output.Stdout,
		Stderr:   output.Stderr,
		ExitCode: output.ExitCode,
	}

	fmt.Printf("  Save test config to %s\n", expected_output_file)
	SaveExpected(expected_output_file, new_config)
}

func SaveExpected(fn string, config TestConfig) {
	ostream, err := os.Create(fn)
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		if err = ostream.Close(); err != nil {
			log.Fatal(err)
		}
	}()

	encoder := json.NewEncoder(ostream)
	err = encoder.Encode(&config)
	if err != nil {
		log.Fatal(err)
	}
}

func usage() {
	fmt.Println("Usage:")
	fmt.Println("    gorth_test test|record [path]")
	fmt.Println()
	fmt.Println("Arguments:")
	fmt.Println("    path - if full - process all dirs. Othwewise it is treated as path to file or directory")
}

func main() {
	if len(os.Args) < 2 {
		usage()
		os.Exit(1)
	}
	command := os.Args[1]

	var input_paths []string
	if len(os.Args) == 2 {
		input_paths = []string{
			"Gorth/tests",
			"Gorth/examples",
			"Gorth/euler",
		}
	} else {
		input_paths = os.Args[2:]
	}

	switch command {
	case "test":
		fmt.Printf("Run tests from %v\n", input_paths)
		TestInputs(PrepareInputs(input_paths))
	case "record":
		RecordTestOutputs(PrepareInputs(input_paths))
	default:
		panic(fmt.Sprintf("Unknown command: %s\n", command))
	}
}
