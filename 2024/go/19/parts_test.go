package day19_test

import (
	day19 "aoc2024/19"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 19, Test: 1, Part: day19.Part1, Expected: "6"},
		{Day: 19, Test: 1, Part: day19.Part2, Expected: "16"},
	})
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 19, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day19.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 19, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day19.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
