package aocmath_test

import (
	aocmath "aoc2024/lib/math"
	"fmt"
	"math"
	"testing"
)

func TestBase10Digits(t *testing.T) {

	for p := range 19 {
		n := uint(math.Pow10(p))
		target := len(fmt.Sprintf("%d", n))
		real := aocmath.Base10Digits(n)
		if target != real {
			panic("wrong")
		}
	}
}
