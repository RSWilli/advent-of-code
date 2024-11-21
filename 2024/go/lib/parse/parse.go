package aocparse

// DecimalDigit parses the given ASCII byte to a 0-9 decimal int
func DecimalDigit(b byte) int {
	return int(b - byte('0'))
}

func Decimal(b []byte) int {
	s := 0

	for _, b := range b {
		s *= 10
		s += int(b)
	}

	return s
}
