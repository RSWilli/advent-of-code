package day00_test

import (
	day00 "aoc2025/00"
	"testing"
)

var test = `
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
	expect := "1227775554"
	if res := day00.Part1(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}

func TestPart2(t *testing.T) {
	expect := "4174379265"
	if res := day00.Part2(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}
