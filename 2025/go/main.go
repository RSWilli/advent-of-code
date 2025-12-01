package main

import (
	day00 "aoc2025/00"
	day01 "aoc2025/01"
	"aoc2025/lib/aocruntime"
	"flag"
)

var day = flag.Int("day", 0, "the day to run, defaults to today")

func main() {
	flag.Parse()

	switch *day {
	case 0:
		// today:
		aocruntime.RunDay(day00.GetInput, day00.Part1, day00.Part2)
	case 1:
		aocruntime.RunDay(day01.GetInput, day01.Part1, day01.Part2)
	default:
		panic("Unknown day")
	}
}
