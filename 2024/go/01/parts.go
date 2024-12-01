package day01

import (
	aocinput "aoc2024/lib/input"
	aocmath "aoc2024/lib/math"
	aocparse "aoc2024/lib/parse"
	"bytes"
	"fmt"
	"iter"
	"slices"
)

type item struct {
	left  int
	right int
}

func parse(r aocinput.Reader) (iter.Seq[item], error) {
	lines, err := r.ReadFileLines()

	if err != nil {
		return nil, err
	}

	return func(yield func(item) bool) {
		for _, l := range lines {
			if len(l) == 0 {
				break
			}

			firstSpace := bytes.IndexByte(l, ' ')
			lastspace := firstSpace + 2

			if firstSpace == -1 {
				panic("wrong input format")
			}

			leftDigits := l[:firstSpace]
			rightDigits := l[lastspace+1:]

			if !yield(item{
				left:  aocparse.Decimal(leftDigits),
				right: aocparse.Decimal(rightDigits),
			}) {
				break
			}
		}
	}, nil
}

func Part1(r aocinput.Reader) (string, error) {
	lines, err := parse(r)

	if err != nil {
		return "", err
	}

	var left []int
	var right []int

	for item := range lines {
		left = append(left, item.left)
		right = append(right, item.right)
	}

	slices.Sort(left)
	slices.Sort(right)

	sumOfDistances := 0

	for i, l := range left {
		r := right[i]

		sumOfDistances += aocmath.Abs(l - r)
	}

	return fmt.Sprintf("%d", sumOfDistances), nil
}

func Part2(r aocinput.Reader) (string, error) {
	lines, err := parse(r)

	if err != nil {
		return "", err
	}

	var left []int
	rightCounts := make(map[int]int)

	for item := range lines {
		left = append(left, item.left)
		rightCounts[item.right]++
	}

	similarityScore := 0

	for _, l := range left {
		similarityScore += l * rightCounts[l]
	}

	return fmt.Sprintf("%d", similarityScore), nil
}
