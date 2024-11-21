package day00

import (
	aocinput "aoc2024/lib/input"
	"fmt"
	"iter"
)

func parse(r aocinput.Reader) (iter.Seq2[int, []byte], error) {
	return r.ReadFileLines()
}

func Part1(r aocinput.Reader) (string, error) {
	lines, err := parse(r)

	if err != nil {
		return "", err
	}

	for _, line := range lines {
		println(string(line))
	}

	return "foobar", nil
}

func Part2(r aocinput.Reader) (string, error) {
	return "", fmt.Errorf("unimplemented")
}
