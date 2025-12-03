package day03_test

import (
	day03 "aoc2025/03"
	"testing"
)

var test = `987654321111111
811111111111119
234234234234278
818181911112111
`

func TestCases(t *testing.T) {
	cases := []struct {
		in     string
		amount int
		expect int
	}{
		{"987654321111111", 2, 98},
		{"811111111111119", 2, 89},
		{"234234234234278", 2, 78},
		{"818181911112111", 2, 92},

		{"987654321111111", 12, 987654321111},
		{"811111111111119", 12, 811111111119},
		{"234234234234278", 12, 434234234278},
		{"818181911112111", 12, 888911112111},
	}

	for _, test := range cases {
		if res := day03.MaximumJoltage(test.in, test.amount); res != test.expect {
			t.Fatalf("expected %d, got %d for %s, %d", test.expect, res, test.in, test.amount)
		}
	}
}

func TestPart1(t *testing.T) {
	expect := "357"
	if res := day03.Part1(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}

func TestPart2(t *testing.T) {
	expect := "3121910778619"
	if res := day03.Part2(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}
