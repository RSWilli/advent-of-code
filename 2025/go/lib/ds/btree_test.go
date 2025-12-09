package aocds_test

import (
	aocds "aoc2025/lib/ds"
	"fmt"
	"testing"
)

func TestBTree(t *testing.T) {
	bt := aocds.BTree[int, struct{}]{}

	bt.Set(10, struct{}{})
	bt.Set(20, struct{}{})
	bt.Set(5, struct{}{})
	bt.Set(6, struct{}{})
	bt.Set(12, struct{}{})
	bt.Set(30, struct{}{})
	bt.Set(7, struct{}{})
	bt.Set(17, struct{}{})

	for k, v := range bt.Entries() {
		fmt.Print(k, v, ",")
	}
	fmt.Println()

}

func TestBTreeDoubleInsert(t *testing.T) {
	bt := aocds.BTree[int, int]{}

	for range 5 {
		bt.InsertOrUpdate(10, 1, func(old int) int {
			return old + 1
		})
	}

	for k, v := range bt.Entries() {
		fmt.Print(k, ":", v, ",")
	}
	fmt.Println()

}
