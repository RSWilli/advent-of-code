package day04

import (
	aocinput "aoc2024/lib/input"
	"fmt"
	"iter"
)

type TwoD = []string

func parse(r aocinput.Reader) ([]string, error) {
	lines, err := r.ReadFileLines()

	if err != nil {
		return nil, err
	}

	var field []string

	for _, line := range lines {
		// must copy, line will be reused by scanner
		field = append(field, string(line))
	}

	return field, nil
}

type position struct {
	x int
	y int
}

func positionIter(sx, sy, dx, dy, len int) iter.Seq2[int, position] {
	return func(yield func(int, position) bool) {
		for i := range len {
			if !yield(i, position{
				x: sx + i*dx,
				y: sy + i*dy,
			}) {
				break
			}
		}
	}
}

const xmas = "XMAS"

func countXmas(x, y int, field TwoD) int {
	if field[y][x] != 'X' {
		return 0
	}

	xmascounts := 0

	directions := []position{
		{0, 1},
		{0, -1},
		{1, 0},
		{-1, 0},

		{1, 1},
		{1, -1},
		{-1, -1},
		{-1, 1},
	}

offsets:
	for _, dir := range directions {
		if x+3*dir.x < 0 || x+3*dir.x >= len(field[y]) || y+3*dir.y < 0 || y+3*dir.y >= len(field) {
			continue
		}

		for i, pos := range positionIter(x, y, dir.x, dir.y, 4) {
			if field[pos.y][pos.x] != xmas[i] {
				continue offsets
			}
		}

		xmascounts++
	}

	return xmascounts
}

func isMASCross(x, y int, field TwoD) bool {
	if field[y][x] != 'A' {
		return false
	}

	if x == 0 || len(field[y])-1 <= x {
		return false
	}

	if y == 0 || len(field)-1 <= y {
		return false
	}

	if !(field[y-1][x-1] == 'M' && field[y+1][x+1] == 'S') && !(field[y-1][x-1] == 'S' && field[y+1][x+1] == 'M') {
		return false
	}

	if !(field[y+1][x-1] == 'M' && field[y-1][x+1] == 'S') && !(field[y+1][x-1] == 'S' && field[y-1][x+1] == 'M') {
		return false
	}

	return true
}

func Part1(r aocinput.Reader) (string, error) {
	field, err := parse(r)

	if err != nil {
		return "", err
	}

	count := 0

	for y, line := range field {
		for x := range line {
			count += countXmas(x, y, field)
		}
	}

	return fmt.Sprintf("%d", count), nil
}

func Part2(r aocinput.Reader) (string, error) {
	field, err := parse(r)

	if err != nil {
		return "", err
	}

	count := 0

	for y, line := range field {
		for x := range line {
			if isMASCross(x, y, field) {
				count++
			}
		}
	}

	return fmt.Sprintf("%d", count), nil
}
