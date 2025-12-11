package main

import (
	day00 "aoc2025/00"
	day01 "aoc2025/01"
	day02 "aoc2025/02"
	day03 "aoc2025/03"
	day04 "aoc2025/04"
	day05 "aoc2025/05"
	day06 "aoc2025/06"
	day07 "aoc2025/07"
	day08 "aoc2025/08"
	day09 "aoc2025/09"
	day10 "aoc2025/10"
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
	case 5:
		aocruntime.RunDay(day05.GetInput, day05.Part1, day05.Part2)
	case 6:
		aocruntime.RunDay(day06.GetInput, day06.Part1, day06.Part2)
	case 7:
		aocruntime.RunDay(day07.GetInput, day07.Part1, day07.Part2)
	case 8:
		aocruntime.RunDay(day08.GetInput, day08.Part1, day08.Part2)
	case 9:
		aocruntime.RunDay(day09.GetInput, day09.Part1, day09.Part2)
	case 10:
		aocruntime.RunDay(day10.GetInput, day10.Part1, day10.Part2)
	default:
		panic("Unknown day")
	}
}
