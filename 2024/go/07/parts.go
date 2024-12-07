package day07

import (
	aocinput "aoc2024/lib/input"
	"fmt"
	"slices"
)

type equation struct {
	result uint
	nums   []uint
}

func parse(r aocinput.Reader) ([]equation, error) {
	parser, err := r.GetParser()

	if err != nil {
		return nil, err
	}

	var equations []equation

	for !parser.AtEOF() {
		eq := equation{}
		res, ok := parser.GetDecimal()

		if !ok {
			return nil, fmt.Errorf("wrong input format, exptected number")
		}

		eq.result = uint(res)

		if !parser.Skip(':') {
			return nil, fmt.Errorf("wrong input format, exptected \":\"")
		}

		for {
			if !parser.Skip(' ') {
				break
			}

			num, ok := parser.GetDecimal()

			if !ok {
				return nil, fmt.Errorf("wrong input format, exptected number")
			}

			eq.nums = append(eq.nums, uint(num))
		}

		if !parser.Skip('\n') {
			break
		}

		equations = append(equations, eq)
	}
	return equations, nil

}

func Concat(a, b uint) uint {
	var f uint = 1

	for f <= b {
		f *= 10
	}

	return a*f + b
}

func Part1(r aocinput.Reader) (string, error) {
	equations, err := parse(r)

	if err != nil {
		return "", err
	}

	var sum uint = 0

	for _, equation := range equations {
		possibleResults := []uint{
			equation.nums[0],
		}

		result := equation.result

		for _, a := range equation.nums[1:] {
			lastresults := make([]uint, 0, len(possibleResults)*2)
			for _, r := range possibleResults {
				if result <= r {
					continue
				}

				lastresults = append(lastresults, r+a, r*a)
			}

			possibleResults = lastresults
		}

		if slices.Contains(possibleResults, result) {
			sum += result
		}
	}

	return fmt.Sprintf("%d", sum), nil
}

func Part2(r aocinput.Reader) (string, error) {
	equations, err := parse(r)

	if err != nil {
		return "", err
	}

	var sum uint = 0

	for _, equation := range equations {
		possibleResults := []uint{
			equation.nums[0],
		}

		result := equation.result

		for _, a := range equation.nums[1:] {
			lastresults := make([]uint, 0, len(possibleResults)*3)

			for _, r := range possibleResults {
				if result <= r {
					continue
				}

				lastresults = append(lastresults, r+a, r*a, Concat(r, a))
			}

			possibleResults = lastresults
		}

		if slices.Contains(possibleResults, result) {
			sum += result
		}
	}

	return fmt.Sprintf("%d", sum), nil
}
