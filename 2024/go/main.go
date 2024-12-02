package main

import (
	day00 "aoc2024/00"
	day01 "aoc2024/01"
	day02 "aoc2024/02"
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
	default:
		panic("unknown day")
	}
}
