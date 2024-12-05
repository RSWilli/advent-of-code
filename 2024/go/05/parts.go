package day05

import (
	aocinput "aoc2024/lib/input"
	"fmt"
	"slices"
)

type orderingRule struct {
	before int
	after  int
}

type printQueue struct {
	orderingRules map[orderingRule]struct{}

	pageUpdates [][]int
}

func parse(r aocinput.Reader) (printQueue, error) {
	parser, err := r.GetParser()

	if err != nil {
		return printQueue{}, err
	}

	q := printQueue{
		orderingRules: make(map[orderingRule]struct{}),
	}

	for {
		a, ok := parser.GetDecimal()

		if !ok {
			break // end of ordering rules
		}

		if !parser.Skip('|') {
			return printQueue{}, fmt.Errorf("wrong input format, expected |")
		}

		b, ok := parser.GetDecimal()

		if !ok {
			return printQueue{}, fmt.Errorf("wrong input format, expected number")
		}

		if !parser.Skip('\n') {
			return printQueue{}, fmt.Errorf("wrong input format, expected newline, got %s", string(parser.Preview(10)))
		}

		q.orderingRules[orderingRule{
			before: a,
			after:  b,
		}] = struct{}{}
	}

	if !parser.Skip('\n') {
		return printQueue{}, fmt.Errorf("wrong input format, expected newline, got %s", string(parser.Preview(10)))
	}

lists:
	for {
		var nums []int
		for {
			n, ok := parser.GetDecimal()

			if !ok {
				break lists
			}

			nums = append(nums, n)

			if !parser.Skip(',') {
				break
			}
		}

		q.pageUpdates = append(q.pageUpdates, nums)

		if !parser.Skip('\n') {
			return printQueue{}, fmt.Errorf("wrong input format, expected newline")
		}
	}

	if !parser.AtEOF() {
		return printQueue{}, fmt.Errorf("wrong input format, expected EOF, got %s", string(parser.Preview(10)))
	}

	return q, nil
}

func cmpFunc(orderingRules map[orderingRule]struct{}) func(a int, b int) int {
	return func(a, b int) int {
		_, aBeforeB := orderingRules[orderingRule{
			before: a,
			after:  b,
		}]

		if aBeforeB {
			return -1
		}

		return 1
	}
}

func middle(ints []int) int {
	return ints[len(ints)/2]
}

func Part1(r aocinput.Reader) (string, error) {
	q, err := parse(r)

	cmp := cmpFunc(q.orderingRules)

	if err != nil {
		return "", err
	}

	middleSum := 0

	for _, line := range q.pageUpdates {
		if slices.IsSortedFunc(line, cmp) {
			middleSum += middle(line)
		}
	}

	return fmt.Sprintf("%d", middleSum), nil
}

func Part2(r aocinput.Reader) (string, error) {
	q, err := parse(r)

	cmp := cmpFunc(q.orderingRules)

	if err != nil {
		return "", err
	}

	middleSum := 0

	for _, line := range q.pageUpdates {
		if !slices.IsSortedFunc(line, cmp) {
			slices.SortFunc(line, cmp)
			middleSum += middle(line)
		}
	}

	return fmt.Sprintf("%d", middleSum), nil
}
