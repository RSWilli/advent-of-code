package day14_test

import (
	day14 "aoc2024/14"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 14, Test: 1, Part: day14.Part1Test, Expected: "12"},
		{Day: 14, Test: 0, UseRealInput: true, Part: day14.Part2, Expected: ""}, // no test for part 2
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 14, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day14.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}

func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 14, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day14.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
