package aocmath

import "math/bits"

func Abs(x int) int {
	if x < 0 {
		return -x
	}

	return x
}

func Sign(x int) int {
	switch {
	case x < 0:
		return -1
	case x == 0:
		return 0
	case x > 0:
		return 1
	}

	return x
}

func Gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func Lcm(a, b int) int {
	return Abs(a*b) / Gcd(a, b)
}

// log2ToLog10 is a static lookup table that maps the base 2 logarithm to
// the floored base 10
var log2ToLog10 = [...]int{
	0, 0, 0, 0, 1, 1, 1, 2,
	2, 2, 3, 3, 3, 3, 4, 4,
	4, 5, 5, 5, 6, 6, 6, 6,
	7, 7, 7, 8, 8, 8, 9, 9,
	9, 9, 10, 10, 10, 11, 11,
	11, 12, 12, 12, 12, 13, 13,
	13, 14, 14, 14, 15, 15, 15,
	15, 16, 16, 16, 17, 17, 17,
	18, 18, 18, 18,
}

// tenPowers is a static lookup table for base 10 powers
var tenPowers = [...]uint{
	1,
	10,
	100,
	1000,
	10000,
	100000,
	1000000,
	10000000,
	100000000,
	1000000000,
	10000000000,
	100000000000,
	1000000000000,
	10000000000000,
	100000000000000,
	1000000000000000,
	10000000000000000,
	100000000000000000,
	1000000000000000000,
	10000000000000000000,
}

// Base10Digits returns the length of the given uint in base 10
//
// inspired by https://stackoverflow.com/questions/25892665/performance-of-log10-function-returning-an-int
func Base10Digits(n uint) int {
	base2Digits := bits.Len(n)

	power := log2ToLog10[base2Digits]

	if n >= tenPowers[power] {
		return power + 1
	}

	return power
}

func Pow10(n int) uint {
	return tenPowers[n]
}
