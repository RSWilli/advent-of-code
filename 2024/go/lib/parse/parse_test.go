package aocparse_test

import (
	aocparse "aoc2024/lib/parse"
	"fmt"
	"testing"
)

func TestDecimal(t *testing.T) {
	num := 97836459
	s := fmt.Sprintf("%d", num)

	out := aocparse.Decimal([]byte(s))
	if out != num {
		t.Fatalf("expected: %d, got: %d", num, out)
	}
}

func TestFloat(t *testing.T) {
	num := 9783.6459
	s := fmt.Sprintf("%f", num)

	out := aocparse.Float([]byte(s))
	if out != num {
		t.Fatalf("expected: %f, got: %f", num, out)
	}
}
