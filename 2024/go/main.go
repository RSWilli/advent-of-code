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
	day10 "aoc2024/10"
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
	case 10:
		r.RunDay(day10.Part1, day10.Part2)
	default:
		panic("unknown day")
	}
}
