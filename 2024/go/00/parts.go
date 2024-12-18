package day00

import (
	aocinput "aoc2024/lib/input"
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
		_ = line
	}

	return "", nil
}

func Part2(r aocinput.Reader) (string, error) {
	return "", nil
}
