package day05_test

import (
	day05 "aoc2025/05"
	"testing"
)

var test = `3-5
10-14
16-20
12-18

1
5
8
11
17
32
`

func TestOverlap(t *testing.T) {
	cases := []struct {
		one    day05.Range
		two    day05.Range
		expect bool
	}{
		{one: day05.Range{5, 10}, two: day05.Range{7, 12}, expect: true},
		{one: day05.Range{7, 12}, two: day05.Range{5, 10}, expect: true},

		{one: day05.Range{1, 12}, two: day05.Range{20, 30}, expect: false},
		{one: day05.Range{11, 12}, two: day05.Range{5, 10}, expect: false},
	}

	for _, test := range cases {
		if res := test.one.Overlaps(test.two); res != test.expect {
			t.Fatalf("expected %t, got %t for %#v %#v", test.expect, res, test.one, test.two)
		}
	}
}

func TestUnion(t *testing.T) {
	cases := []struct {
		one    day05.Range
		two    day05.Range
		expect day05.Range
	}{
		{one: day05.Range{5, 10}, two: day05.Range{7, 12}, expect: day05.Range{5, 12}},
		{one: day05.Range{7, 12}, two: day05.Range{5, 10}, expect: day05.Range{5, 12}},
	}

	for _, test := range cases {
		if res := test.one.Union(test.two); res != test.expect {
			t.Fatalf("expected %#v, got %#v for %#v %#v", test.expect, res, test.one, test.two)
		}
	}
}

func TestPart1(t *testing.T) {
	expect := "3"
	if res := day05.Part1(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}

func TestPart2(t *testing.T) {
	expect := "14"
	if res := day05.Part2(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}
