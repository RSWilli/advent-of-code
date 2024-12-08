package day08

import (
	aocinput "aoc2024/lib/input"
	"aoc2024/lib/twodimensional"
	"fmt"
)

func parse(r aocinput.Reader) (map[byte][]twodimensional.Position, int, int, error) {
	lines, err := r.ReadFileLines()

	if err != nil {
		return nil, 0, 0, err
	}

	antennas := make(map[byte][]twodimensional.Position)

	maxX := 0
	maxY := 0

	for y, line := range lines {
		maxY = max(maxY, y)
		for x, a := range line {
			maxX = max(maxX, x)

			if a == '.' {
				continue
			}

			antennas[a] = append(antennas[a], twodimensional.Position{
				X: x,
				Y: y,
			})
		}
	}

	return antennas, maxX, maxY, nil
}

func Part1(r aocinput.Reader) (string, error) {
	antennas, maxX, maxY, err := parse(r)

	if err != nil {
		return "", err
	}

	antinodes := make(map[twodimensional.Position]struct{})

	for _, positions := range antennas {
		for i, p1 := range positions {
			for j, p2 := range positions {
				if i == j {
					continue
				}

				dist := p1.Distance(p2)

				a1 := p2.Add(dist)
				a2 := p1.Add(dist.Negate())

				if a1.InBounds(0, maxX, 0, maxY) {
					antinodes[a1] = struct{}{}
				}

				if a2.InBounds(0, maxX, 0, maxY) {
					antinodes[a2] = struct{}{}
				}
			}
		}
	}

	return fmt.Sprintf("%d", len(antinodes)), nil
}

func Part2(r aocinput.Reader) (string, error) {
	antennas, maxX, maxY, err := parse(r)

	if err != nil {
		return "", err
	}

	antinodes := make(map[twodimensional.Position]struct{})

	for _, positions := range antennas {
		for i, p1 := range positions {
			for j, p2 := range positions {
				if i == j {
					continue
				}

				dist := p1.Distance(p2).Normalize()

				castRay(
					p1,
					dist,
					maxX,
					maxY,
					antinodes,
				)

				castRay(
					p1,
					dist.Negate(),
					maxX,
					maxY,
					antinodes,
				)
			}
		}
	}

	return fmt.Sprintf("%d", len(antinodes)), nil
}

func castRay(
	from twodimensional.Position,
	d twodimensional.Vector,
	maxX,
	maxY int,
	antinodes map[twodimensional.Position]struct{},
) {
	current := from

	for current.InBounds(0, maxX, 0, maxY) {
		antinodes[current] = struct{}{}

		current = current.Add(d)
	}
}
