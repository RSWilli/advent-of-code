package day10

import (
	aocinput "aoc2024/lib/input"
	aocparse "aoc2024/lib/parse"
	aocsearch "aoc2024/lib/search"
	"aoc2024/lib/twodimensional"
	"fmt"
	"sync"
	"sync/atomic"
)

func parse(r aocinput.Reader) (twodimensional.Field[int], error) {
	lines, err := r.ReadFileLines()

	if err != nil {
		return nil, err
	}

	var f twodimensional.Field[int]

	for _, line := range lines {
		var row []int
		for _, digit := range line {
			row = append(row, aocparse.DecimalDigit(digit))
		}

		f = append(f, row)
	}

	return f, nil
}

func trailScoreFrom(start twodimensional.Position, f twodimensional.Field[int]) int {
	return aocsearch.BFSCountPaths[twodimensional.Position](
		start,
		func(p twodimensional.Position) []twodimensional.Position {
			current := f.Lookup(p)

			neighs := p.Neighbors()

			filtered := make([]twodimensional.Position, 0, 4)

			for _, n := range neighs {
				if f.Has(n) && f.Lookup(n) == current+1 {
					filtered = append(filtered, n)
				}
			}

			return filtered
		},
		func(p twodimensional.Position) bool {
			return f.Lookup(p) == 9
		},
	)
}

func Part1(r aocinput.Reader) (string, error) {
	field, err := parse(r)

	if err != nil {
		return "", err
	}

	var totalScore atomic.Int64

	var wg sync.WaitGroup

	for y, line := range field {
		for x, d := range line {
			if d != 0 {
				continue
			}

			wg.Add(1)

			go func() {
				score := trailScoreFrom(twodimensional.Position{X: x, Y: y}, field)

				totalScore.Add(int64(score))

				wg.Done()
			}()
		}
	}

	wg.Wait()

	return fmt.Sprintf("%d", totalScore.Load()), nil
}

func trailRatingFrom(start twodimensional.Position, f twodimensional.Field[int]) int {
	return aocsearch.DFSCountPaths[twodimensional.Position](
		start,
		func(p twodimensional.Position) []twodimensional.Position {
			current := f.Lookup(p)

			neighs := p.Neighbors()

			filtered := make([]twodimensional.Position, 0, 4)

			for _, n := range neighs {
				if f.Has(n) && f.Lookup(n) == current+1 {
					filtered = append(filtered, n)
				}
			}

			return filtered
		},
		func(p twodimensional.Position) bool {
			return f.Lookup(p) == 9
		},
	)
}

func Part2(r aocinput.Reader) (string, error) {
	field, err := parse(r)

	if err != nil {
		return "", err
	}

	var totalRating atomic.Int64

	var wg sync.WaitGroup

	for y, line := range field {
		for x, d := range line {
			if d != 0 {
				continue
			}

			wg.Add(1)

			go func() {
				rating := trailRatingFrom(twodimensional.Position{X: x, Y: y}, field)

				totalRating.Add(int64(rating))

				wg.Done()
			}()
		}
	}

	wg.Wait()

	return fmt.Sprintf("%d", totalRating.Load()), nil
}
