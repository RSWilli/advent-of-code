package day12_test

import (
	day12 "aoc2024/12"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 12, Test: 1, Part: day12.Part1, Expected: "1930"},
		{Day: 12, Test: 1, Part: day12.Part2, Expected: "1206"},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 12, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day12.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 12, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day12.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
