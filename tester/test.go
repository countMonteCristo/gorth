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

const GorthExec = "bin/gorth"

type TestStatus int

const (
	StatusSuccess TestStatus = iota
	StatusFail
	StatusSkip
)

type Stats struct {
	Total    int
	Detailed map[TestStatus]int
}

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

type TestResult struct {
	status TestStatus
	msg    string
}

func NewTestCase(fn string) TestCase {
	return TestCase{
		File: fn,
		Config: TestConfig{
			Argv: make([]string, 0), Stdin: "", Stdout: "", Stderr: "", ExitCode: 0,
		},
		Cmd: []string{
			GorthExec,
			"-O", "1",
			fn,
		},
	}
}

func (t *TestCase) GetExpectedFilePath() string {
	ext := filepath.Ext(t.File)
	name := t.File[:len(t.File)-len(ext)]
	return name + ".txt"
}

// TODO: update input and output separately
func (t *TestCase) run() TestOutput {
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

func (t *TestCase) Check(output TestOutput, result *TestResult) {
	result.status = StatusFail

	stdout_ok := CheckOutput(output.Stdout, t.Config.Stdout, "STDOUT", result)
	stderr_ok := CheckOutput(output.Stderr, t.Config.Stderr, "STDERR", result)
	exit_code_ok := (output.ExitCode == t.Config.ExitCode)

	if !exit_code_ok {
		result.msg += fmt.Sprintf(
			"  EXITCODE differs:"+
				"\tActual:   %d\n"+
				"\tExpected: %d\n",
			output.ExitCode, t.Config.ExitCode,
		)
	}

	if stdout_ok && stderr_ok && exit_code_ok {
		result.msg += "SUCCESS\n"
		result.status = StatusSuccess
	} else {
		result.msg += "FAILED\n"
		result.status = StatusFail
	}
}

func CheckOutput(actual, expected, out_desc string, result *TestResult) bool {
	if actual != expected {
		result.msg += fmt.Sprintf(
			"  %s differs:\n"+
				"\tActual:   `%v`\n"+
				"\tExpected: `%v`\n",
			out_desc, actual, expected,
		)
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

func TestFile(fn string, results chan TestResult) {
	testcase := NewTestCase(fn)
	expected_output_file := testcase.GetExpectedFilePath()

	var result TestResult
	exists := testcase.LoadExpected(expected_output_file)

	result.msg += fmt.Sprintf("Running testcase %s\n", fn)
	if !exists {
		result.status = StatusSkip
		result.msg += fmt.Sprintf(
			"  Config file %s not found, skip testcase\nSKIP\n",
			expected_output_file,
		)
	} else {
		output := testcase.run()
		testcase.Check(output, &result)
	}
	results <- result
}

func TestInputs(gorth_fns []string) {
	stats := Stats{Detailed: map[TestStatus]int{StatusSuccess: 0, StatusSkip: 0, StatusFail: 0}}

	results_chan := make(chan TestResult)
	for _, gorth_fn := range gorth_fns {
		go TestFile(gorth_fn, results_chan)
	}

	count := 0
	for {
		result := <-results_chan
		count++

		stats.Total++
		stats.Detailed[result.status]++

		fmt.Fprint(os.Stderr, result.msg)

		if count == len(gorth_fns)-1 {
			close(results_chan)
			break
		}
	}

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
	encoder.SetIndent("", "  ")
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

	if _, err := os.Stat(GorthExec); errors.Is(err, os.ErrNotExist) {
		cmd := exec.Command("go", "build", "-o", GorthExec, "gorth.go")
		cmd.Start()
		cmd.Wait()
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
