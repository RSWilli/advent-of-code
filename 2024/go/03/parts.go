package day03

import (
	aocinput "aoc2024/lib/input"
	"fmt"
	"iter"
)

type command struct {
	a int
	b int
}

func parse(r aocinput.Reader, conditional bool) (iter.Seq[command], error) {
	parser, err := r.GetParser()

	if err != nil {
		return nil, err
	}

	return func(yield func(command) bool) {
		enabled := true

		for !parser.AtEOF() {
			if conditional && parser.SkipPrefix([]byte("do()")) {
				enabled = true

				continue
			}

			if conditional && parser.SkipPrefix([]byte("don't()")) {
				enabled = false

				continue
			}

			if !enabled || !parser.SkipPrefix([]byte("mul(")) {
				parser.Advance()

				continue
			}

			a, ok := parser.GetDecimal()

			if !ok {
				continue
			}

			if !parser.Skip(',') {
				continue
			}

			b, ok := parser.GetDecimal()

			if !ok {
				continue
			}

			if !parser.Skip(')') {
				continue
			}

			if !yield(command{
				a: a,
				b: b,
			}) {
				break
			}
		}

	}, nil
}

func Part1(r aocinput.Reader) (string, error) {
	cmds, err := parse(r, false)

	if err != nil {
		return "", err
	}

	sum := 0

	for cmd := range cmds {
		sum += cmd.a * cmd.b
	}

	return fmt.Sprintf("%d", sum), nil
}

func Part2(r aocinput.Reader) (string, error) {
	cmds, err := parse(r, true)

	if err != nil {
		return "", err
	}

	sum := 0

	for cmd := range cmds {
		sum += cmd.a * cmd.b
	}

	return fmt.Sprintf("%d", sum), nil
}
