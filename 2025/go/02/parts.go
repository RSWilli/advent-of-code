package day02

import (
	"aoc2025/lib/aocapi"
	"fmt"
	"strconv"
	"strings"
)

func GetInput() string {
	i, err := aocapi.GetInput(2025, 2)

	if err != nil {
		panic(err)
	}

	return i
}

func IsInvalidIDPart1(s string) bool {
	first := s[:len(s)/2]
	second := s[len(s)/2:]

	if len(first) != len(second) {
		return false
	}

	for i := range len(first) {
		if first[i] != second[i] {
			return false
		}
	}
	return true
}

func IsInvalidIDPart2(s string) bool {
nextchunk:
	for chunkSize := 1; chunkSize <= len(s)/2; chunkSize++ {
		if len(s)%chunkSize != 0 {
			// doesn't divide evenly
			continue nextchunk
		}

		reference := s[:chunkSize]

		for chunk := range len(s) / chunkSize {
			offset := chunk * chunkSize
			currentChunk := s[offset : offset+chunkSize]

			if currentChunk != reference {
				continue nextchunk
			}
		}

		return true
	}

	return false
}

func Part1(in string) string {
	ranges := strings.Split(in, ",")

	sum := 0

	for _, r := range ranges {
		r = strings.Trim(r, "\n")
		parts := strings.Split(r, "-")

		start, _ := strconv.Atoi(parts[0])
		end, _ := strconv.Atoi(parts[1])

		for i := range end - start + 1 {
			current := start + i

			if IsInvalidIDPart1(fmt.Sprintf("%d", current)) {
				sum += current
			}
		}
	}

	return fmt.Sprintf("%d", sum)
}

func Part2(in string) string {
	ranges := strings.Split(in, ",")

	sum := 0

	for _, r := range ranges {
		r = strings.Trim(r, "\n")
		parts := strings.Split(r, "-")

		start, _ := strconv.Atoi(parts[0])
		end, _ := strconv.Atoi(parts[1])

		for i := range end - start + 1 {
			current := start + i

			if IsInvalidIDPart2(fmt.Sprintf("%d", current)) {
				sum += current
			}
		}
	}

	return fmt.Sprintf("%d", sum)
}
