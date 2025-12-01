package aocmath

// Abs returns the absolut value of the given int. Go stdlib only provides this for floats
func Abs(x int) int {
	if x < 0 {
		return -x
	}

	return x
}

// Mod returns the modulo of the number in the given ring
func Mod(a, b int) int {
	mod := a % b

	if mod < 0 {
		return mod + b
	}

	return mod
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
