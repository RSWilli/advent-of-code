package day01

import (
	"aoc2025/lib/aocapi"
	"aoc2025/lib/aocmath"
	"fmt"
	"strconv"
	"strings"
)

func GetInput() string {
	i, err := aocapi.GetInput(2025, 1)

	if err != nil {
		panic(err)
	}

	return i
}

const startDial = 50

const dialSize = 100

func Part1(in string) string {
	lines := strings.Split(in, "\n")

	lines = lines[:len(lines)-1]

	zeroes := 0

	current := startDial

	for _, line := range lines {
		dir := line[0]

		amount, err := strconv.Atoi(line[1:])

		if err != nil {
			panic(err)
		}

		if dir == 'L' {
			current = (current - amount) % dialSize
		} else {
			current = (current + amount) % dialSize
		}

		if current == 0 {
			zeroes++
		}

	}

	return fmt.Sprintf("%d", zeroes)
}

func Part2(in string) string {
	lines := strings.Split(in, "\n")

	lines = lines[:len(lines)-1]

	zeroes := 0

	current := startDial

	for _, line := range lines {
		dir := line[0]

		amount, err := strconv.Atoi(line[1:])

		if err != nil {
			panic(err)
		}

		before := current

		// full rotations
		zeroes += amount / dialSize

		if dir == 'L' {
			amount = -amount
		}

		current = aocmath.Mod(current+amount, dialSize)

		// hit 0 exactly
		if current == 0 {
			zeroes++
		}

		// did not start at zero but wrapped around
		if before != 0 && dir == 'L' && before < current {
			zeroes++
		}
		// did not end at zero but wrapped around
		if current != 0 && dir == 'R' && before > current {
			zeroes++
		}
	}

	return fmt.Sprintf("%d", zeroes)
}
