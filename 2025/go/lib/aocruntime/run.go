package aocruntime

import (
	"fmt"
	"time"
)

type InputFunc func() string
type AOCFunc func(string) string

func RunDay(in InputFunc, p1, p2 AOCFunc) {
	input := in()

	runPart(1, input, p1)
	runPart(2, input, p2)
}

func runPart(p int, in string, part AOCFunc) {
	defer func() {
		// print panic
		if r := recover(); r != nil {
			fmt.Printf("Part %d: panic: %v\n", p, r)
		}
	}()

	now := time.Now()
	result := part(in)
	elapsed := time.Since(now)

	fmt.Printf("Part %d: %s (took %s)\n", p, result, elapsed)
}
