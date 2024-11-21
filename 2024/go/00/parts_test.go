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
