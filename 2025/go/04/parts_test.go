package day04_test

import (
	day04 "aoc2025/04"
	"testing"
)

var test = `
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
`

func TestPart1(t *testing.T) {
	expect := "13"
	if res := day04.Part1(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}

func TestPart2(t *testing.T) {
	expect := "43"
	if res := day04.Part2(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}
