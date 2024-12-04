package day04_test

import (
	day04 "aoc2024/04"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 4, Test: 1, Part: day04.Part1, Expected: "18"},
		{Day: 4, Test: 1, Part: day04.Part2, Expected: "9"},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 4, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day04.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 4, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day04.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
