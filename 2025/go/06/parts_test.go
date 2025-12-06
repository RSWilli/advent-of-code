package day06_test

import (
	day06 "aoc2025/06"
	"testing"
)

var test = `
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
`

func TestPart1(t *testing.T) {
	expect := "4277556"
	if res := day06.Part1(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}

func TestPart2(t *testing.T) {
	expect := "3263827"
	if res := day06.Part2(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}
