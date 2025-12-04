package day04

import (
	"aoc2025/lib/aocapi"
	"aoc2025/lib/twodimensional"
	"fmt"
)

func GetInput() string {
	i, err := aocapi.GetToday()

	if err != nil {
		panic(err)
	}

	return i
}

func accessiblePaperRolls(lines twodimensional.Field[byte]) map[twodimensional.Position]struct{} {
	accessiblePaperRolls := make(map[twodimensional.Position]struct{})

	for y, line := range lines {
		for x := range line {
			pos := twodimensional.Position{X: x, Y: y}

			if lines[pos.Y][pos.X] != '@' {
				continue // no paper roll
			}

			neighs := pos.NeighborsDiag()

			neighborRolls := 0

			for _, neigh := range neighs {
				if !lines.Has(neigh) {
					continue
				}

				if lines.Lookup(neigh) == '@' {
					neighborRolls++
				}
			}

			if neighborRolls < 4 {
				accessiblePaperRolls[pos] = struct{}{}
			}
		}
	}

	return accessiblePaperRolls
}

func Part1(in string) string {
	lines := twodimensional.NewFieldFromLines(in)

	return fmt.Sprintf("%d", len(accessiblePaperRolls(lines)))
}

func Part2(in string) string {
	lines := twodimensional.NewFieldFromLines(in)

	totalAccessible := 0

	for {
		toRemove := accessiblePaperRolls(lines)

		if len(toRemove) == 0 {
			break
		}

		totalAccessible += len(toRemove)

		for pos := range toRemove {
			lines.Set(pos, '.')
		}
	}

	return fmt.Sprintf("%d", totalAccessible)
}
