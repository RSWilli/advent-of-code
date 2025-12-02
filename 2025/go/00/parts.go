package day00

import (
	"aoc2025/lib/aocapi"
	"fmt"
)

func GetInput() string {
	i, err := aocapi.GetToday()

	if err != nil {
		panic(err)
	}

	return i
}

func Part1(in string) string {
	fmt.Println(in)
	panic("unimplemented")
}

func Part2(in string) string {
	panic("unimplemented")
}
