package day12

import (
	aocinput "aoc2024/lib/input"
	"aoc2024/lib/twodimensional"
	"bytes"
	"fmt"
)

func parse(r aocinput.Reader) (twodimensional.Field[byte], error) {
	lines, err := r.ReadFileLines()

	if err != nil {
		return nil, err
	}

	garden := make(twodimensional.Field[byte], 0, 100)

	for _, line := range lines {
		// must copy, line will be reused by scanner
		garden = append(garden, bytes.Clone(line))
	}

	return garden, nil
}

func regionPrizePerimeter(label byte, start twodimensional.Position, garden twodimensional.Field[byte], seen map[twodimensional.Position]struct{}) int {
	todo := make([]twodimensional.Position, 0, 100)

	todo = append(todo, start)
	seen[start] = struct{}{}

	area := 0
	perimeter := 0

	for len(todo) > 0 {
		next := todo[0]
		todo = todo[1:]

		area++

		for _, n := range next.Neighbors() {
			if !garden.Has(n) {
				perimeter++
				continue
			}

			neighborLabel := garden.Lookup(n)

			_, visited := seen[n]

			if visited && neighborLabel == label {
				continue
			}

			if neighborLabel == label {
				seen[n] = struct{}{}
				todo = append(todo, n)
			} else {
				perimeter++
			}
		}
	}

	return area * perimeter
}

func Part1(r aocinput.Reader) (string, error) {
	garden, err := parse(r)

	if err != nil {
		return "", err
	}

	seen := make(map[twodimensional.Position]struct{})

	totalPrize := 0

	for y, line := range garden {
		for x, c := range line {
			pos := twodimensional.Position{X: x, Y: y}
			if _, ok := seen[pos]; !ok {
				totalPrize += regionPrizePerimeter(c, pos, garden, seen)
			}
		}
	}

	return fmt.Sprintf("%d", totalPrize), nil
}

func is(garden twodimensional.Field[byte], pos twodimensional.Position, label byte) bool {
	return garden.Has(pos) && garden.Lookup(pos) == label
}

func countCorners(label byte, pos twodimensional.Position, garden twodimensional.Field[byte]) int {
	corners := 0

	top := pos.Walk(twodimensional.DirectionUp)
	topleft := top.Walk(twodimensional.DirectionLeft)
	topright := top.Walk(twodimensional.DirectionRight)
	bottom := pos.Walk(twodimensional.DirectionDown)
	bottomleft := bottom.Walk(twodimensional.DirectionLeft)
	bottomright := bottom.Walk(twodimensional.DirectionRight)
	left := pos.Walk(twodimensional.DirectionLeft)
	right := pos.Walk(twodimensional.DirectionRight)

	if is(garden, top, label) && is(garden, left, label) && !is(garden, topleft, label) {
		// inside topleft:
		//  #
		// ##
		corners++
	}

	if is(garden, top, label) && is(garden, right, label) && !is(garden, topright, label) {
		// inside topright:
		// #
		// ##
		corners++
	}

	if is(garden, bottom, label) && is(garden, left, label) && !is(garden, bottomleft, label) {
		// inside bottomleft:
		// ##
		//  #
		corners++
	}

	if is(garden, bottom, label) && is(garden, right, label) && !is(garden, bottomright, label) {
		// inside bottomright:
		// ##
		// #
		corners++
	}

	if !is(garden, left, label) && !is(garden, top, label) {
		// outside topleft:
		// ##
		// #
		corners++
	}

	if !is(garden, right, label) && !is(garden, top, label) {
		// outside topright:
		// ##
		//  #
		corners++
	}

	if !is(garden, left, label) && !is(garden, bottom, label) {
		// outside bottomleft:
		// #
		// ##
		corners++
	}

	if !is(garden, right, label) && !is(garden, bottom, label) {
		// outside bottomright:
		//  #
		// ##
		corners++
	}

	return corners
}

func regionPrizeEdges(label byte, start twodimensional.Position, garden twodimensional.Field[byte], seen map[twodimensional.Position]struct{}) int {
	todo := make([]twodimensional.Position, 0, 100)

	todo = append(todo, start)
	seen[start] = struct{}{}

	area := 0
	corners := 0

	for len(todo) > 0 {
		next := todo[0]
		todo = todo[1:]

		area++

		corners += countCorners(label, next, garden)

		for _, n := range next.Neighbors() {
			if !garden.Has(n) {
				continue
			}

			neighborLabel := garden.Lookup(n)

			_, visited := seen[n]

			if visited {
				continue
			}

			if neighborLabel == label {
				seen[n] = struct{}{}
				todo = append(todo, n)
			}
		}
	}

	return area * corners
}

func Part2(r aocinput.Reader) (string, error) {
	garden, err := parse(r)

	if err != nil {
		return "", err
	}

	seen := make(map[twodimensional.Position]struct{})

	totalPrize := 0

	for y, line := range garden {
		for x, c := range line {
			pos := twodimensional.Position{X: x, Y: y}
			if _, ok := seen[pos]; !ok {
				totalPrize += regionPrizeEdges(c, pos, garden, seen)
			}
		}
	}

	return fmt.Sprintf("%d", totalPrize), nil
}
