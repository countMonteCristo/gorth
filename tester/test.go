package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
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

type TestConfig struct {
	Argv     []string
	Stdin    string
	Stdout   string
	Stderr   string
	ExitCode int
}

func (tc *TestConfig) Argc() int {
	return len(tc.Argv)
}

type TestCase struct {
	File   string
	Cmd    []string
	Config TestConfig
}

// TODO: provide stdin for tested script
func (t *TestCase) run() (status int) {
	fmt.Printf("Running testcase %s...\n", t.File)
	status = StatusFail

	cmd := exec.Command(t.Cmd[0], t.Cmd[1:]...)

	var outbuf strings.Builder
	var errbuf strings.Builder
	cmd.Stdout = &outbuf
	cmd.Stderr = &errbuf

	cmd.Run()
	stdout := outbuf.String()
	stderr := errbuf.String()
	exit_code := cmd.ProcessState.ExitCode()

	stdout_ok := CheckOutput(stdout, t.Config.Stdout, "STDOUT")
	stderr_ok := CheckOutput(stderr, t.Config.Stderr, "STDERR")
	exit_code_ok := (exit_code == t.Config.ExitCode)

	if !exit_code_ok {
		fmt.Fprintln(os.Stderr, "  EXITCODE differs:")
		fmt.Fprintf(os.Stderr, "\tActual:   %d\n", exit_code)
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

func (t *TestCase) LoadExpected(fn string) {
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

	var status int
	if _, err := os.Stat(expected_output_file); errors.Is(err, os.ErrNotExist) {
		status = StatusSkip
		fmt.Fprintf(os.Stderr, "Running testcase %s...\n", fn)
		fmt.Fprintf(os.Stderr, "  Config file %s not found, skip testcase\n", expected_output_file)
		fmt.Println("SKIP")
	} else {
		testcase := TestCase{
			File: fn,
		}
		testcase.LoadExpected(expected_output_file)
		testcase.Cmd = []string{
			"go", "run", "gorth.go", fn,
		}
		testcase.Cmd = append(testcase.Cmd, testcase.Config.Argv...)
		status = testcase.run()
	}

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
	full_flag := flag.Bool("full", false, "run all availiable tests")
	flag.Parse()

	var input_paths []string
	if *full_flag {
		input_paths = make([]string, 0)
		input_paths = append(
			input_paths,
			"Gorth/tests",
			"Gorth/examples",
			"Gorth/euler",
		)
		if len(flag.Args()) > 0 {
			fmt.Println("WARNING: Input paths are ignored because of -full flag")
		}
	} else {
		input_paths = flag.Args()
	}

	fmt.Printf("Run tests from %v\n", input_paths)
	TestInputs(PrepareInputs(input_paths))
}
