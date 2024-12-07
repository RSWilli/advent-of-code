package day07_test

import (
	day07 "aoc2024/07"
	aocinput "aoc2024/lib/input"
	"testing"
)

func Test(t *testing.T) {
	aocinput.RunTests(t, []aocinput.Test{
		{Day: 7, Test: 1, Part: day07.Part1, Expected: "3749"},
		{Day: 7, Test: 1, Part: day07.Part2, Expected: "11387"},
	})
}

func TestConcat(t *testing.T) {
	if day07.Concat(12, 345) != 12345 {
		panic("wrong")
	}
	if day07.Concat(12, 1) != 121 {
		panic("wrong")
	}
}

func BenchmarkPart1(b *testing.B) {
	r := aocinput.Reader{Day: 7, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day07.Part1(r)

		if err != nil {
			panic(err)
		}
	}
}
func BenchmarkPart2(b *testing.B) {
	r := aocinput.Reader{Day: 7, Test: false}

	for n := 0; n < b.N; n++ {
		_, err := day07.Part2(r)

		if err != nil {
			panic(err)
		}
	}
}
