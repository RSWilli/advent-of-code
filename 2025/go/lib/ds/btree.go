package aocds

import (
	"cmp"
	"iter"
)

// BTree implementation inspired by https://www.geeksforgeeks.org/dsa/insert-operation-in-b-tree/
type BTree[K cmp.Ordered, V any] struct {
	root *bTreeNode[K, V]
}

func (bt *BTree[K, V]) Entries() iter.Seq2[K, V] {
	return func(yield func(K, V) bool) {
		if bt.root == nil {
			return
		}

		childIter := bt.root.entries()

		childIter(yield)
	}
}

// Find returns the value for the given key and whether it was found
func (bt *BTree[K, V]) Find(key K) (v V, ok bool) {
	if bt.root == nil {
		return
	}

	kv := bt.root.search(key)

	if kv == nil {
		return
	}

	return kv.value, true
}

// Set sets the given key to the value, overwriting any existing values
func (bt *BTree[K, V]) Set(key K, value V) {
	bt.InsertOrUpdate(key, value, func(_ V) V {
		return value
	})
}

func (bt *BTree[K, V]) InsertOrUpdate(key K, value V, updateFunc func(old V) V) {
	if bt.root == nil {
		bt.insertNew(key, value)
		return
	}

	// find the kv and update it if not found

	kv := bt.root.search(key)

	if kv == nil {
		bt.insertNew(key, value)
		return
	}

	kv.value = updateFunc(kv.value)
}

// insertNew inserts a kv pair that wasn't previously in the [BTree]
func (bt *BTree[K, V]) insertNew(key K, value V) {
	if bt.root == nil {
		bt.root = &bTreeNode[K, V]{
			leaf: true,
		}
		bt.root.values[0] = bTreeKV[K, V]{key: key, value: value}
		bt.root.n = 1
	} else if bt.root.n == maxDegree {
		newroot := &bTreeNode[K, V]{
			leaf: false,
		}
		newroot.children[0] = bt.root

		newroot.splitchild(0, bt.root)

		// newroot now has two children and a single kv pair, and we need to
		// decide whether to insert left or right of it
		i := 0
		if newroot.values[0].key < key {
			i++
		}

		newroot.children[i].insertNonFull(key, value)

		bt.root = newroot
	} else {
		bt.root.insertNonFull(key, value)
	}
}

type bTreeKV[K cmp.Ordered, V any] struct {
	key   K
	value V
}

const degree = 3

const maxDegree = 2*degree - 1

type bTreeNode[K cmp.Ordered, V any] struct {
	leaf     bool
	values   [maxDegree]bTreeKV[K, V]
	children [maxDegree + 1]*bTreeNode[K, V]
	n        int
}

func (btn *bTreeNode[K, V]) entries() iter.Seq2[K, V] {
	return func(yield func(K, V) bool) {
		// There are n keys and n+1 children, traverse through n keys
		// and first n children
		for i := range btn.n {
			if !btn.leaf {
				// first traverse the children

				childIter := btn.children[i].entries()

				childIter(yield)
			}

			if !yield(btn.values[i].key, btn.values[i].value) {
				return
			}
		}

		if !btn.leaf {
			// iterate last child:
			childIter := btn.children[btn.n].entries()

			childIter(yield)
		}
	}
}

func (btn *bTreeNode[K, V]) search(key K) *bTreeKV[K, V] {
	i := 0

	for i < btn.n && key > btn.values[i].key {
		i++
	}

	if i < btn.n && btn.values[i].key == key {
		// found in this node
		return &btn.values[i]
	}

	if btn.leaf {
		// no children and not found
		return nil
	}

	// search children
	return btn.children[i].search(key)
}

// splitchild splits the child node of btn on index i. The child must be full.
func (btn *bTreeNode[K, V]) splitchild(index int, old *bTreeNode[K, V]) {
	node := &bTreeNode[K, V]{
		leaf: old.leaf,
		n:    degree - 1,
	}

	// copy the last values into the new one
	copy(node.values[:], old.values[degree:])
	clear(old.values[degree:])

	if !old.leaf {
		// copy the last children into the new one
		copy(node.children[:], old.children[degree:])
		clear(old.children[degree:])
	}

	old.n = degree - 1

	// move our children one spot to the right if after the index:
	for i := btn.n; i >= index+1; i-- {
		btn.children[i+1] = btn.children[i]
	}

	// add the new child to this node
	btn.children[index+1] = node

	// move our values one spot to the right if after the index:
	for i := btn.n; i >= index; i-- {
		btn.values[i+1] = btn.values[i]
	}

	// copy the middle value to this node:
	btn.values[index] = old.values[degree-1]

	// we now have more keys
	btn.n++
}

// insertNonFull adds a new key and value
func (btn *bTreeNode[K, V]) insertNonFull(key K, value V) {
	// start at the max key
	i := btn.n - 1
	if btn.leaf {
		// find the space where the new key belongs and copy the values to the right
		for i >= 0 && btn.values[i].key > key {
			btn.values[i+1] = btn.values[i]
			i--
		}

		btn.values[i+1] = bTreeKV[K, V]{key: key, value: value}
		btn.n++
	} else {
		for i >= 0 && btn.values[i].key > key {
			i--
		}

		// i-th child can be full:
		if btn.children[i+1].n == maxDegree {
			btn.splitchild(i+1, btn.children[i+1])

			// after the split i+1 or i+2 can have the key
			if btn.values[i+1].key < key {
				i++
			}
		}

		btn.children[i+1].insertNonFull(key, value)
	}
}
