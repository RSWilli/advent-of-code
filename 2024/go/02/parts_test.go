package day02_test

import (
	day02 "aoc2024/02"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 2, Test: 1, Part: day02.Part1, Expected: "2"},
		{Day: 2, Test: 1, Part: day02.Part2, Expected: "4"},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 2, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day02.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 2, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day02.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
