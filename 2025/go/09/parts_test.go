package day09_test

import (
	day00 "aoc2025/00"
	"testing"
)

var test = `
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
`

// func TestCases(t *testing.T) {
// 	cases := []struct {
// 		in     string
// 		expect bool
// 	}{}

// 	for _, test := range cases {
// 		if res := day00.Foo(test.in); res != test.expect {
// 			t.Fatalf("expected %t, got %t for %s", test.expect, res, test.in)
// 		}
// 	}
// }

func TestPart1(t *testing.T) {
	expect := "50"
	if res := day00.Part1(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}

func TestPart2(t *testing.T) {
	expect := "24"
	if res := day00.Part2(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}
