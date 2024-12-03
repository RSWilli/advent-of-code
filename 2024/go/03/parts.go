package day03

import (
	aocinput "aoc2024/lib/input"
	aocparse "aoc2024/lib/parse"
	"bytes"
	"fmt"
	"iter"
	"regexp"
)

var cmdReg = regexp.MustCompile(`mul\(\d+,\d+\)|do\(\)|don't\(\)`)

type command struct {
	enabled bool
	a       int
	b       int
}

func parse(r aocinput.Reader) (iter.Seq[command], error) {
	tokens, err := r.ReadFileSplit(func(data []byte, atEOF bool) (advance int, token []byte, err error) {
		if atEOF && len(data) == 0 {
			return 0, nil, nil
		}

		first := cmdReg.FindIndex(data)

		if first == nil {
			// Request more data.
			return 0, nil, nil
		}

		start := first[0]
		end := first[1]

		return end, data[start:end], nil
	})

	if err != nil {
		return nil, err
	}

	return func(yield func(command) bool) {

		enabled := true
		for _, cmd := range tokens {
			cmd = bytes.TrimSuffix(cmd, []byte(")"))

			mulcmd, ok := bytes.CutPrefix(cmd, []byte("mul("))

			if ok {
				nums := bytes.Split(mulcmd, []byte(","))

				if !yield(command{
					enabled: enabled,
					a:       aocparse.Decimal(nums[0]),
					b:       aocparse.Decimal(nums[1]),
				}) {
					break
				}

				continue
			}

			if bytes.HasPrefix(cmd, []byte("do(")) {
				enabled = true
				continue
			}

			if bytes.HasPrefix(cmd, []byte("don't(")) {
				enabled = false
				continue
			}

			panic("unexpected cmd: " + string(cmd))

		}
	}, nil
}

func Part1(r aocinput.Reader) (string, error) {
	cmds, err := parse(r)

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
	cmds, err := parse(r)

	if err != nil {
		return "", err
	}

	sum := 0

	for cmd := range cmds {
		if cmd.enabled {
			sum += cmd.a * cmd.b
		}
	}

	return fmt.Sprintf("%d", sum), nil
}
