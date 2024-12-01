package day01_test

import (
	day01 "aoc2024/01"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 1, Test: 1, Part: day01.Part1, Expected: "11"},
		{Day: 1, Test: 1, Part: day01.Part2, Expected: "31"},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 1, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day01.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 1, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day01.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
