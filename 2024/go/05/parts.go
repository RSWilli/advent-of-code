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

func middle(ints []int) int {
	return ints[len(ints)/2]
}

func Part1(r aocinput.Reader) (string, error) {
	q, err := parse(r)

	if err != nil {
		return "", err
	}

	middleSum := 0

nextline:
	for _, line := range q.pageUpdates {
		for i, a := range line[:len(line)-1] {
			for _, b := range line[i+1:] {
				_, wrong := q.orderingRules[orderingRule{
					before: b,
					after:  a,
				}]

				if wrong {
					// fmt.Printf("%v is wrong because of %d|%d\n", line, b, a)

					continue nextline
				}
			}
		}

		middleSum += middle(line)
	}

	return fmt.Sprintf("%d", middleSum), nil
}

func order(ints []int, orderingRules map[orderingRule]struct{}) {
	slices.SortFunc(ints, func(a, b int) int {
		_, aBeforeB := orderingRules[orderingRule{
			before: a,
			after:  b,
		}]

		if aBeforeB {
			return -1
		}

		_, bBeforea := orderingRules[orderingRule{
			before: b,
			after:  a,
		}]

		if bBeforea {
			return 1
		}

		return 0
	})
}

func Part2(r aocinput.Reader) (string, error) {
	q, err := parse(r)

	if err != nil {
		return "", err
	}

	middleSum := 0

nextline:
	for _, line := range q.pageUpdates {
		for i, a := range line[:len(line)-1] {
			for _, b := range line[i+1:] {
				_, wrong := q.orderingRules[orderingRule{
					before: b,
					after:  a,
				}]

				if wrong {
					// fmt.Printf("%v is wrong because of %d|%d\n", line, b, a)

					order(line, q.orderingRules)

					middleSum += middle(line)
					continue nextline
				}
			}
		}

	}

	return fmt.Sprintf("%d", middleSum), nil
}
