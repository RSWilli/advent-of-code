package day02

import (
	aocinput "aoc2024/lib/input"
	aocmath "aoc2024/lib/math"
	aocparse "aoc2024/lib/parse"
	"fmt"
	"iter"
)

func parse(r aocinput.Reader) (iter.Seq[[]int], error) {
	lines, err := r.ReadFileLines()

	if err != nil {
		return nil, err
	}

	return func(yield func([]int) bool) {
		for _, l := range lines {
			sequence := make([]int, 0, 20)

			for _, num := range aocparse.Split(l, ' ') {
				sequence = append(sequence, aocparse.Decimal(num))
			}

			if !yield(sequence) {
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

	for line := range lines {
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
	for line := range lines {
		if isSafeLevels(line) {
			safeLines++
			continue
		}

		for i := range line {
			dampened := append(line[0:i:i], line[i+1:]...)

			if isSafeLevels(dampened) {
				safeLines++
				continue lines
			}
		}
	}

	return fmt.Sprintf("%d", safeLines), nil
}
