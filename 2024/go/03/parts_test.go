package day03_test

import (
	day03 "aoc2024/03"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 3, Test: 1, Part: day03.Part1, Expected: "161"},
		{Day: 3, Test: 2, Part: day03.Part2, Expected: "48"},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 3, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day03.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 3, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day03.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
