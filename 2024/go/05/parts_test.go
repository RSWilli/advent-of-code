package day05_test

import (
	day05 "aoc2024/05"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 5, Test: 1, Part: day05.Part1, Expected: "143"},
		{Day: 5, Test: 1, Part: day05.Part2, Expected: "123"},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 5, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day05.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 5, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day05.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
