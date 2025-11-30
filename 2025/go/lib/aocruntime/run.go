package aocruntime

import "fmt"

type InputFunc func() string
type AOCFunc func(string) string

func RunDay(in InputFunc, p1, p2 AOCFunc) {
	input := in()

	fmt.Printf("Part1: %s\n", p1(input))
	fmt.Printf("Part2: %s\n", p2(input))
}
