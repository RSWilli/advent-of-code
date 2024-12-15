package day15_test

import (
	day15 "aoc2024/15"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 15, Test: 1, Part: day15.Part1, Expected: "2028"},
		{Day: 15, Test: 2, Part: day15.Part1, Expected: "10092"},
		{Day: 15, Test: 1, Part: day15.Part2, Expected: ""},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 15, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day15.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 15, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day15.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
