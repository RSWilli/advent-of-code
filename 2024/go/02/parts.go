package day02

import (
	aocinput "aoc2024/lib/input"
	aocmath "aoc2024/lib/math"
	aocparse "aoc2024/lib/parse"
	"bytes"
	"fmt"
	"iter"
)

func parse(r aocinput.Reader) (iter.Seq2[int, []int], error) {
	lines, err := r.ReadFileLines()

	if err != nil {
		return nil, err
	}

	return func(yield func(int, []int) bool) {
		for i, l := range lines {

			digits := bytes.Split(l, []byte(" "))

			sequence := make([]int, 0, len(digits))

			for _, num := range digits {
				sequence = append(sequence, aocparse.Decimal(num))
			}

			if !yield(i, sequence) {
				break
			}
		}
	}, nil
}

func isSafeLevels(line []int) bool {
	signSet := false
	sign := 0

	for i, a := range line[:len(line)-1] {
		b := line[i+1]

		diff := a - b

		abs := aocmath.Abs(diff)

		if abs == 0 || abs > 3 {
			return false
		}

		s := aocmath.Sign(diff)

		if signSet && sign != s {
			return false
		}

		sign = s
		signSet = true
	}

	return true
}

func Part1(r aocinput.Reader) (string, error) {
	lines, err := parse(r)

	if err != nil {
		return "", err
	}

	safeLines := 0

	for _, line := range lines {
		if isSafeLevels(line) {
			safeLines++
		}
	}

	return fmt.Sprintf("%d", safeLines), nil
}

func Part2(r aocinput.Reader) (string, error) {
	lines, err := parse(r)

	if err != nil {
		return "", err
	}

	safeLines := 0

lines:
	for _, line := range lines {
		if isSafeLevels(line) {
			safeLines++
			continue
		}

		for i := range line {
			dampened := make([]int, 0, len(line))
			dampened = append(dampened, line[0:i]...)
			dampened = append(dampened, line[i+1:]...)

			if isSafeLevels(dampened) {
				safeLines++
				continue lines
			}
		}
	}

	return fmt.Sprintf("%d", safeLines), nil
}
