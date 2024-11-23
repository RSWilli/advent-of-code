package aocparse

import (
	"bytes"
	"slices"
)

// DecimalDigit parses the given ASCII byte to a 0-9 decimal int
func DecimalDigit(b byte) int {
	return int(b - byte('0'))
}

func Decimal(b []byte) int {
	s := 0

	for _, b := range b {
		s *= 10
		s += DecimalDigit(b)
	}

	return s
}

func Float(b []byte) float64 {
	i := bytes.IndexByte(b, '.')

	if i == -1 {
		return float64(Decimal(b))
	}

	whole := float64(Decimal(b[0:i]))

	fraction := 0.0

	for _, b := range slices.Backward(b[i+1:]) {
		fraction += float64(DecimalDigit(b))
		fraction /= 10
	}

	return whole + fraction
}
