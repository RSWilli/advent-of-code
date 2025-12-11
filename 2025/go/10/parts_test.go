package day10_test

import (
	day10 "aoc2025/10"
	"strings"
	"testing"
)

var test = `
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
`

func TestParsing(t *testing.T) {
	cases := strings.SplitSeq(strings.Trim(test, "\n"), "\n")

	for test := range cases {
		if res := day10.ParseMachine(test); res.String() != test {
			t.Fatalf("expected:\n%s\ngot:\n%s", test, res.String())
		}
	}
}

func TestPart1(t *testing.T) {
	expect := "7"
	if res := day10.Part1(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}

func TestPart2(t *testing.T) {
	expect := "4174379265"
	if res := day10.Part2(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}
