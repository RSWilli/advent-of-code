package main

import (
	day00 "aoc2024/00"
	day01 "aoc2024/01"
	day02 "aoc2024/02"
	day03 "aoc2024/03"
	day04 "aoc2024/04"
	day05 "aoc2024/05"
	day06 "aoc2024/06"
	day07 "aoc2024/07"
	day08 "aoc2024/08"
	day09 "aoc2024/09"
	day10 "aoc2024/10"
	day11 "aoc2024/11"
	day13 "aoc2024/13"
	day14 "aoc2024/14"
	day15 "aoc2024/15"
	aocinput "aoc2024/lib/input"
	"flag"
)

func main() {
	day := flag.Int("day", 0, "the day to run")
	test := flag.Bool("test", false, "use tests instead of real inputs")
	testnr := flag.Int("testnr", 1, "test file to use")

	flag.Parse()

	r := aocinput.Reader{
		Day:    *day,
		Test:   *test,
		Testnr: *testnr,
	}

	switch *day {
	case 0:
		r.RunDay(day00.Part1, day00.Part2)
	case 1:
		r.RunDay(day01.Part1, day01.Part2)
	case 2:
		r.RunDay(day02.Part1, day02.Part2)
	case 3:
		r.RunDay(day03.Part1, day03.Part2)
	case 4:
		r.RunDay(day04.Part1, day04.Part2)
	case 5:
		r.RunDay(day05.Part1, day05.Part2)
	case 6:
		r.RunDay(day06.Part1, day06.Part2)
	case 7:
		r.RunDay(day07.Part1, day07.Part2)
	case 8:
		r.RunDay(day08.Part1, day08.Part2)
	case 9:
		r.RunDay(day09.Part1, day09.Part2)
	case 10:
		r.RunDay(day10.Part1, day10.Part2)
	case 11:
		r.RunDay(day11.Part1, day11.Part2)
	case 13:
		r.RunDay(day13.Part1, day13.Part2)
	case 14:
		r.RunDay(day14.Part1, day14.Part2)
	case 15:
		r.RunDay(day15.Part1, day15.Part2)
	default:
		panic("unknown day")
	}
}
