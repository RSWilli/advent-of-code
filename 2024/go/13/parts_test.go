package day13_test

import (
	day13 "aoc2024/13"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 13, Test: 1, Part: day13.Part1, Expected: "480"},
		{Day: 13, Test: 1, Part: day13.Part2, Expected: ""},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 13, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day13.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 13, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day13.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
