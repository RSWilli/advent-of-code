package aocinput

import (
	"bufio"
	"bytes"
	"fmt"
	"iter"
	"os"
	"path/filepath"
)

type Reader struct {
	Day    int
	Test   bool
	Testnr int
}

func (r Reader) fileName() string {
	dir := os.Getenv("WORKSPACE_DIR")

	if dir == "" {
		panic("expected WORKSPACE_DIR env var not found, please set to project root")
	}

	dir = filepath.Join(dir, "2024", "go")

	if r.Test {
		return filepath.Join(dir, "tests", fmt.Sprintf("day%02d_%d.txt", r.Day, r.Testnr))
	}

	return filepath.Join(dir, "inputs", fmt.Sprintf("day%02d.txt", r.Day))
}

func (r Reader) OpenInput() (*os.File, error) {
	fname := r.fileName()

	f, err := os.Open(fname)

	if err != nil {
		return nil, err
	}

	return f, nil
}

func (r Reader) ReadFileSplit(split bufio.SplitFunc) (iter.Seq2[int, []byte], error) {
	f, err := r.OpenInput()

	if err != nil {
		f.Close()

		return nil, err
	}

	scanner := bufio.NewScanner(f)
	scanner.Split(split)

	return func(yield func(int, []byte) bool) {
		defer f.Close()
		line := 0

		for scanner.Scan() {
			if !yield(line, scanner.Bytes()) {
				break
			}

			line++
		}
	}, nil
}

func (r Reader) ReadFileLines() (iter.Seq2[int, []byte], error) {
	return r.ReadFileSplit(bufio.ScanLines)
}

func GetSeparatedBySplitFunc(seperator []byte) bufio.SplitFunc {
	n := len(seperator)
	if n == 0 {
		return bufio.ScanRunes
	}

	return func(data []byte, atEOF bool) (advance int, token []byte, err error) {
		if atEOF && len(data) == 0 {
			return 0, nil, nil
		}

		if i := bytes.Index(data, seperator); i >= 0 {
			return i + n, data[0:i], nil
		}

		// If we're at EOF, we have a final, non-separated segment. Return it.
		if atEOF {
			return len(data), data, nil
		}

		// Request more data.
		return 0, nil, nil
	}
}

func (r Reader) ReadFileSeparatedBy(sep []byte) (iter.Seq2[int, []byte], error) {
	return r.ReadFileSplit(GetSeparatedBySplitFunc(sep))
}

type AOCFunc func(Reader) (string, error)

func (r Reader) RunDay(p1, p2 AOCFunc) {
	fmt.Println("----- Part 1 -----")
	out1, err := p1(r)

	if err != nil {
		panic(fmt.Sprintf("got error from part1: %v", err))
	}

	fmt.Printf("Part 1 Result: %s\n", out1)

	fmt.Println("----- Part 2 -----")
	out2, err := p2(r)

	if err != nil {
		panic(fmt.Sprintf("got error from part2: %v", err))
	}

	fmt.Printf("Part 2 Result: %s\n", out2)
}
