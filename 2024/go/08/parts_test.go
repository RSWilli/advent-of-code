package day08_test

import (
	day08 "aoc2024/08"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 8, Test: 1, Part: day08.Part1, Expected: "14"},
		{Day: 8, Test: 1, Part: day08.Part2, Expected: "34"},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 8, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day08.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 8, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day08.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
