package aocinput

import (
	"bufio"
	"fmt"
	"iter"
	"os"
	"path/filepath"
	"runtime"
)

type Reader struct {
	Day    int
	Test   bool
	Testnr int
}

func (r Reader) fileName() string {
	if r.Test {
		// this fixes the lookup of the files because working dir is different
		// this expects that the folder structure doesn't change
		_, file, _, ok := runtime.Caller(0)

		if !ok {
			panic("could not figure out caller location")
		}

		return filepath.Join(file, "..", "..", "..", fmt.Sprintf("./tests/day%02d_%d.txt", r.Day, r.Testnr))
	}

	return fmt.Sprintf("./inputs/day%02d.txt", r.Day)
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

func (r Reader) ReadFileCommaSeparated() (iter.Seq2[int, []byte], error) {
	panic("TODO")
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
