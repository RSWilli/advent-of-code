package day11

import (
	aocinput "aoc2024/lib/input"
	aocmath "aoc2024/lib/math"
	aocparse "aoc2024/lib/parse"
	"bytes"
	"fmt"
)

type stone int

func (s stone) step() []stone {
	if s == 0 {
		return []stone{1}
	}

	digits := aocmath.Base10Digits(uint(s))

	if digits%2 == 0 {
		m := aocmath.Pow10(digits / 2)

		upper := uint(s) / m
		lower := uint(s) % m

		return []stone{stone(upper), stone(lower)}
	}

	return []stone{
		s * 2024,
	}
}

func parse(r aocinput.Reader) (map[stone]int, error) {
	input, err := r.ReadAll()

	if err != nil {
		return nil, err
	}

	stones := make(map[stone]int)

	input = bytes.TrimSuffix(input, []byte("\n"))

	nums := bytes.Split(input, []byte(" "))

	// skip the last char since its a newline
	for _, t := range nums {
		stones[stone(aocparse.Decimal(t))]++
	}

	return stones, nil
}

func step(stones map[stone]int) map[stone]int {
	next := make(map[stone]int, len(stones)*2)

	for stone, count := range stones {
		for _, nextstone := range stone.step() {
			next[nextstone] += count
		}
	}

	return next
}

func run(stones map[stone]int, steps int) int {
	current := stones

	for range steps {
		current = step(current)
	}

	sum := 0

	for _, c := range current {
		sum += c
	}

	return sum
}

func Part1(r aocinput.Reader) (string, error) {
	stones, err := parse(r)

	if err != nil {
		return "", err
	}

	return fmt.Sprintf("%d", run(stones, 25)), nil
}

func Part2(r aocinput.Reader) (string, error) {
	stones, err := parse(r)

	if err != nil {
		return "", err
	}

	return fmt.Sprintf("%d", run(stones, 75)), nil
}
