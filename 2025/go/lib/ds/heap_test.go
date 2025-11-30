package aocds_test

import (
	aocds "aoc2025/lib/ds"
	"slices"
	"testing"
)

func TestHeap(t *testing.T) {
	h := aocds.NewHeap(func(a, b int) bool {
		return a < b
	})

	items := []int{
		4, 5, 6, 2, 4, 8, 97, 0, 3, 23,
	}

	for _, i := range items {
		h.Push(i)
	}

	var out []int

	for h.Len() > 0 {
		out = append(out, h.Pop())
	}

	if !slices.IsSorted(out) {
		t.Fatal(out)
	}
}

func TestHeap2(t *testing.T) {
	h := aocds.NewHeap(func(a, b int) bool {
		return a < b
	})

	h.Push(5)
	h.Pop()
}

func TestHeap3(t *testing.T) {
	h := aocds.NewHeap(func(a, b int) bool {
		return a < b
	})

	h.Push(4)
	h.Push(5)
	h.Push(6)
	h.Push(2)
	h.Push(4)
	h.Push(8)
	h.Push(97)
	h.Push(0)
	h.Push(3)

	var out []int

	for h.Len() > 0 {
		out = append(out, h.Pop())
	}

	if !slices.IsSorted(out) {
		t.Fatal(out)
	}
}
