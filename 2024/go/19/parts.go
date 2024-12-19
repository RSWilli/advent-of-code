package day19

import (
	aocinput "aoc2024/lib/input"
	aocsearch "aoc2024/lib/search"
	"fmt"
	"strings"
)

type onsen struct {
	available []string

	patterns []string
}

func parse(r aocinput.Reader) (onsen, error) {
	lines, err := r.ReadFileLines()

	if err != nil {
		return onsen{}, err
	}

	var o onsen

	for i, line := range lines {
		if len(line) == 0 {
			continue
		}

		if i == 0 {
			o.available = strings.Split(string(line), ", ")
		} else {
			o.patterns = append(o.patterns, string(line))
		}
	}

	return o, nil
}

func Part1(r aocinput.Reader) (string, error) {
	onsen, err := parse(r)

	if err != nil {
		return "", err
	}

	possible := 0

	for _, pat := range onsen.patterns {
		_, found := aocsearch.Astar(
			pat,
			func(s string) (next []aocsearch.EdgeTo[string]) {
				for _, a := range onsen.available {
					rest, ok := strings.CutPrefix(s, a)

					if ok {
						next = append(next, aocsearch.EdgeTo[string]{
							Node: rest,
							Cost: 1,
						})
					}
				}

				return
			},
			func(a string) int {
				return len(a)
			},
			func(s string) bool {
				return len(s) == 0
			},
		)

		if found {
			possible++
		}
	}

	return fmt.Sprintf("%d", possible), nil
}

func Part2(r aocinput.Reader) (string, error) {
	onsen, err := parse(r)

	if err != nil {
		return "", err
	}

	possibilities := 0

	for _, pat := range onsen.patterns {
		paths := aocsearch.DFSCountPaths[string]( // TODO: must memoize
			pat,
			func(s string) (next []string) {
				for _, a := range onsen.available {
					rest, ok := strings.CutPrefix(s, a)

					if ok {
						next = append(next, rest)
					}
				}

				return
			},
			func(s string) bool {
				return len(s) == 0
			},
		)

		possibilities += paths
	}

	return fmt.Sprintf("%d", possibilities), nil
}
