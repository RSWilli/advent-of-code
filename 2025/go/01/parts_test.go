package day01_test

import (
	day01 "aoc2025/01"
	"testing"
)

// func TestPart1(t *testing.T) {
// 	day00.Part1("foo")
// }

const test = `L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
`

const test2 = `R1000
`

const test3 = `R50
`

func TestPart2(t *testing.T) {
	if res := day01.Part2(test); res != "6" {
		panic("wrong: " + res)
	}
}

func TestPart2Full(t *testing.T) {
	if res := day01.Part2(test2); res != "10" {
		panic("wrong: " + res)
	}
}

func TestPart2Edge(t *testing.T) {
	if res := day01.Part2(test3); res != "1" {
		panic("wrong: " + res)
	}
}
