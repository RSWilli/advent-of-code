package day08_test

import (
	day08 "aoc2025/08"
	"testing"
)

var test = `
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
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
	expect := "40"
	if res := day08.Part1Test(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}

func TestPart2(t *testing.T) {
	expect := "25272"
	if res := day08.Part2(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}
