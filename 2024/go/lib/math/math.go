package aocmath

func Abs(x int) int {
	if x < 0 {
		return -x
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
