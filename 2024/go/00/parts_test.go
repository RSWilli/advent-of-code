package day00_test

import (
	day00 "aoc2024/00"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 0, Test: 1, Part: day00.Part1, Expected: ""},
		{Day: 0, Test: 1, Part: day00.Part2, Expected: ""},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 0, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day00.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 0, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day00.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
