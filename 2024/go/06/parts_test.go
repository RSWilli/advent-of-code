package day06_test

import (
	day06 "aoc2024/06"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 6, Test: 1, Part: day06.Part1, Expected: "41"},
		{Day: 6, Test: 1, Part: day06.Part2, Expected: "6"},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 6, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day06.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 6, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day06.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
