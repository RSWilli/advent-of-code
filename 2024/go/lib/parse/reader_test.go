package aocparse_test

import (
	aocparse "aoc2024/lib/parse"
	"strings"
	"testing"
)

func TestReader(t *testing.T) {
	s := strings.NewReader("foobar1337")

	r := aocparse.NewReader(s)

	if !r.SkipPrefix([]byte("foo")) {
		panic("foo")
	}

	if !r.Skip('b') {
		panic("b")
	}

	if !r.Skip('a') {
		panic("a")
	}

	if r.SkipPrefix([]byte("bam")) {
		panic("bam")
	}

	if r.AtEOF() {
		panic("EOF")
	}

	r.Advance()

	if r.Skip('r') {
		panic("a")
	}

	if d, ok := r.GetDecimal(); !ok || d != 1337 {
		panic("1337")
	}

	if !r.AtEOF() {
		panic("EOF")
	}
}
