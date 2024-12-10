package day09_test

import (
	day09 "aoc2024/09"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 9, Test: 1, Part: day09.Part1, Expected: "1928"},
		{Day: 9, Test: 1, Part: day09.Part2, Expected: ""},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 9, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day09.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 9, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day09.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
