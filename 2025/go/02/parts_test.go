package day02_test

import (
	day02 "aoc2025/02"
	"testing"
)

var test = `11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124
`

func TestInvalidIDs(t *testing.T) {
	cases := []struct {
		in     string
		expect bool
	}{
		{"123", false},
		{"121", false},
		{"1212", true},
		{"1000", false},
		{"222220", false},
		{"12123", false},
	}

	for _, test := range cases {
		if res := day02.IsInvalidIDPart1(test.in); res != test.expect {
			t.Fatalf("expected %t, got %t for %s", test.expect, res, test.in)
		}
	}
}
func TestPart1(t *testing.T) {
	expect := "1227775554"
	if res := day02.Part1(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}

func TestPart2(t *testing.T) {
	expect := "4174379265"
	if res := day02.Part2(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}
