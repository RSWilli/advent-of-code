package day14

import (
	aocinput "aoc2024/lib/input"
	"aoc2024/lib/twodimensional"
	"cmp"
	"fmt"
	"slices"
)

type robot struct {
	pos      twodimensional.Position
	velocity twodimensional.Vector
}

func (r robot) step(width, height int) robot {
	newpos := r.pos.Add(r.velocity)
	return robot{
		pos: twodimensional.Position{
			X: ((newpos.X % width) + width) % width,
			Y: ((newpos.Y % height) + height) % height,
		},
		velocity: r.velocity,
	}
}

func parse(r aocinput.Reader) ([]robot, error) {
	p, err := r.GetParser()

	if err != nil {
		return nil, err
	}

	robots := make([]robot, 0, 100)

	for !p.AtEOF() {
		if !p.SkipPrefix([]byte("p=")) {
			return nil, fmt.Errorf("wrong input format, expected p=")
		}

		px, ok := p.GetDecimal()

		if !ok {
			return nil, fmt.Errorf("wrong input format, expected number")
		}

		if !p.Skip(',') {
			return nil, fmt.Errorf("wrong input format, expected ,")
		}

		py, ok := p.GetDecimal()

		if !ok {
			return nil, fmt.Errorf("wrong input format, expected number")
		}

		if !p.SkipPrefix([]byte(" v=")) {
			return nil, fmt.Errorf("wrong input format, expected p=")
		}

		vx, ok := p.GetSignedDecimal()

		if !ok {
			return nil, fmt.Errorf("wrong input format, expected signed number")
		}

		if !p.Skip(',') {
			return nil, fmt.Errorf("wrong input format, expected ,")
		}

		vy, ok := p.GetSignedDecimal()

		if !ok {
			return nil, fmt.Errorf("wrong input format, expected signed number")
		}

		if !p.Skip('\n') {
			return nil, fmt.Errorf("wrong input format, expected newline")
		}

		robots = append(robots, robot{
			pos:      twodimensional.Position{X: px, Y: py},
			velocity: twodimensional.Vector{X: vx, Y: vy},
		})
	}

	return robots, nil
}

func part1GenericSize(r aocinput.Reader, width, height int) (string, error) {
	robots, err := parse(r)

	if err != nil {
		return "", err
	}

	for range 100 {
		nextrobots := make([]robot, 0, len(robots))

		for _, r := range robots {
			nextrobots = append(nextrobots, r.step(width, height))
		}

		robots = nextrobots
	}

	midx := width / 2
	midy := height / 2

	topLeftCount := 0
	topRightCount := 0
	bottomLeftCount := 0
	bottomRightCount := 0

	for _, r := range robots {
		switch {
		case r.pos.X < midx && r.pos.Y < midy:
			topLeftCount += 1
		case r.pos.X > midx && r.pos.Y < midy:
			topRightCount += 1
		case r.pos.X < midx && r.pos.Y > midy:
			bottomLeftCount += 1
		case r.pos.X > midx && r.pos.Y > midy:
			bottomRightCount += 1
		}
	}

	solution := topLeftCount * topRightCount * bottomLeftCount * bottomRightCount

	return fmt.Sprintf("%d", solution), nil
}

// Part1Test is used for the test input since the example uses a different field size than the actual input
func Part1Test(r aocinput.Reader) (string, error) {
	return part1GenericSize(r, 11, 7)
}

const width = 101
const height = 103

func Part1(r aocinput.Reader) (string, error) {
	return part1GenericSize(r, width, height)
}

func print(robots []robot) {
	field := make(twodimensional.Field[rune], height)

	for i := range field {
		field[i] = make([]rune, width)

		for j := range field[i] {
			field[i][j] = '.'
		}
	}

	for _, r := range robots {
		field.Set(r.pos, 'X')
	}

	for _, line := range field {
		for _, c := range line {
			fmt.Print(string(c))
		}
		fmt.Println()
	}
}

func showsChristmasTree(robots []robot) bool {
	slices.SortFunc(robots, func(a, b robot) int {
		if a.pos.X == b.pos.X {
			return cmp.Compare(a.pos.Y, b.pos.Y)
		}

		return cmp.Compare(a.pos.X, b.pos.X)
	})

	line := 0
	lastY := 0
	for _, r := range robots {
		if r.pos.Y == lastY+1 {
			line++
		} else {
			line = 1
		}

		if line > 20 {
			return true
		}

		lastY = r.pos.Y
	}

	return false
}

func Part2(r aocinput.Reader) (string, error) {
	robots, err := parse(r)

	if err != nil {
		return "", err
	}

	i := 0

	for !showsChristmasTree(robots) {
		nextrobots := make([]robot, 0, len(robots))

		for _, r := range robots {
			nextrobots = append(nextrobots, r.step(width, height))
		}

		i++

		if i > 10_000 {
			return "", fmt.Errorf("limit reached")
		}
		robots = nextrobots
	}

	print(robots)

	return fmt.Sprintf("%d", i), nil
}
