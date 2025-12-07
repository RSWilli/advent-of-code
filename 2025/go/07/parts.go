package day07

import (
	"aoc2025/lib/aocapi"
	aocsearch "aoc2025/lib/search"
	"aoc2025/lib/twodimensional"
	"fmt"
	"strings"
)

func GetInput() string {
	i, err := aocapi.GetInput(2025, 7)

	if err != nil {
		panic(err)
	}

	return i
}

func Part1(in string) string {
	field := twodimensional.NewFieldFromLines(strings.Trim(in, "\n"))

	start, _ := field.Find('S')

	splits := 0

	aocsearch.BFS(start,
		func(p twodimensional.Position) []twodimensional.Position {
			next := p.Walk(twodimensional.DirectionDown)

			if !field.Has(next) {
				return nil
			}

			c := field.Lookup(next)

			if c == '^' {
				splits++

				return []twodimensional.Position{
					next.Walk(twodimensional.DirectionLeft),
					next.Walk(twodimensional.DirectionRight),
				}
			}

			return []twodimensional.Position{next}
		},
		func(twodimensional.Position) bool {
			return false
		},
	)

	return fmt.Sprintf("%d", splits)
}

func CountPathsFrom(cache map[twodimensional.Position]int, p twodimensional.Position, f twodimensional.Field[byte]) int {
	if !f.Has(p) {
		return 1
	}

	if cached, ok := cache[p]; ok {
		return cached
	}

	c := f.Lookup(p)

	if c == '^' {
		leftPaths := CountPathsFrom(cache, p.Walk(twodimensional.DirectionLeft), f)
		rightPaths := CountPathsFrom(cache, p.Walk(twodimensional.DirectionRight), f)

		cache[p] = leftPaths + rightPaths

		return leftPaths + rightPaths
	}

	next := p.Walk(twodimensional.DirectionDown)

	return CountPathsFrom(cache, next, f)
}

func Part2(in string) string {
	field := twodimensional.NewFieldFromLines(strings.Trim(in, "\n"))

	start, _ := field.Find('S')

	cache := make(map[twodimensional.Position]int)

	paths := CountPathsFrom(cache, start, field)

	return fmt.Sprintf("%d", paths)
}
