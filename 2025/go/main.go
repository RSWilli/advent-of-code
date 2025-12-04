package main

import (
	day00 "aoc2025/00"
	day01 "aoc2025/01"
	day02 "aoc2025/02"
	day03 "aoc2025/03"
	day04 "aoc2025/04"
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
	case 2:
		aocruntime.RunDay(day02.GetInput, day02.Part1, day02.Part2)
	case 3:
		aocruntime.RunDay(day03.GetInput, day03.Part1, day03.Part2)
	case 4:
		aocruntime.RunDay(day04.GetInput, day04.Part1, day04.Part2)
	default:
		panic("Unknown day")
	}
}
