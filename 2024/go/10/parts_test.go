package day10_test

import (
	day10 "aoc2024/10"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 10, Test: 1, Part: day10.Part1, Expected: "36"},
		{Day: 10, Test: 1, Part: day10.Part2, Expected: "81"},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 10, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day10.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 10, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day10.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
