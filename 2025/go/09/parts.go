package day09

import (
	"aoc2025/lib/aocapi"
	"aoc2025/lib/aocmath"
	"aoc2025/lib/aocparse"
	"aoc2025/lib/twodimensional"
	"fmt"
	"strings"
)

func GetInput() string {
	i, err := aocapi.GetInput(2025, 9)

	if err != nil {
		panic(err)
	}

	return i
}

func Part1(in string) string {
	lines := strings.Split(strings.Trim(in, "\n"), "\n")

	points := make([]twodimensional.Position, 0, len(lines))

	for _, line := range lines {
		parts := strings.Split(line, ",")

		points = append(points, twodimensional.Position{
			X: aocparse.Int(parts[0]),
			Y: aocparse.Int(parts[1]),
		})
	}

	biggestArea := 0

	for _, p1 := range points {
		for _, p2 := range points {
			area := (aocmath.Abs(p1.X-p2.X) + 1) * (aocmath.Abs(p1.Y-p2.Y) + 1)

			biggestArea = max(biggestArea, area)
		}
	}

	return fmt.Sprintf("%d", biggestArea)
}

type Edge struct {
	P1 twodimensional.Position
	P2 twodimensional.Position
}

func (e *Edge) IsHorizontal() bool {
	return e.P1.Y == e.P2.Y
}

func (e *Edge) IsVertical() bool {
	return e.P1.X == e.P2.X
}

func Part2(in string) string {
	lines := strings.Split(strings.Trim(in, "\n"), "\n")

	points := make([]twodimensional.Position, 0, len(lines))

	for _, line := range lines {
		parts := strings.Split(line, ",")

		points = append(points, twodimensional.Position{
			X: aocparse.Int(parts[0]),
			Y: aocparse.Int(parts[1]),
		})
	}

	// var horizontalEdges aocds.BTree[int, []Edge]

	biggestArea := 0

	for _, p1 := range points {
		for _, p2 := range points {
			area := (aocmath.Abs(p1.X-p2.X) + 1) * (aocmath.Abs(p1.Y-p2.Y) + 1)

			if area < biggestArea {
				continue // save on calculating, this will get dropped anyways
			}

			polygon := []twodimensional.Position{
				{X: min(p1.X, p2.Y), Y: min(p1.Y, p2.Y)},
				{X: max(p1.X, p2.Y), Y: min(p1.Y, p2.Y)},
				{X: max(p1.X, p2.Y), Y: max(p1.Y, p2.Y)},
				{X: min(p1.X, p2.Y), Y: max(p1.Y, p2.Y)},
			}

			if !polygonContains(points, polygon) {
				continue
			}

			biggestArea = max(biggestArea, area)
		}
	}

	return fmt.Sprintf("%d", biggestArea)
}

type Polygon = []twodimensional.Position

func polygonContains(poly1, poly2 Polygon) bool {
	panic("unimplemented")
}
