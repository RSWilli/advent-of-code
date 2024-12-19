package day11_test

import (
	day11 "aoc2024/11"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 11, Test: 1, Part: day11.Part1, Expected: "55312"},
		// {Day: 11, Test: 1, Part: day11.Part2, Expected: ""}, // wow no part 2 test
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 11, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day11.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 11, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day11.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}