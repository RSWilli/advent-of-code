package aocds

type Heap[Item any] struct {
	h    []Item
	less func(a, b Item) bool
}

func NewHeap[Item any](lessfunc func(a, b Item) bool) *Heap[Item] {
	return &Heap[Item]{
		h:    make([]Item, 0, 20),
		less: lessfunc,
	}
}

func (h *Heap[Item]) Len() int {
	return len(h.h)
}

func (h *Heap[Item]) Push(i Item) {
	h.h = append(h.h, i)

	h.up(h.Len() - 1)
}

func (h *Heap[Item]) Peek() Item {
	return h.h[0]
}

func (h *Heap[Item]) Pop() Item {
	switch len(h.h) {
	case 0:
		panic("emtpy heap")
	case 1:
		out := h.h[0]

		h.h = h.h[1:]

		return out
	default:
		// cannot remove root only, as this would destroy the parent relations
		// must swap to back!
		h.swap(0, h.Len()-1)

		out := h.h[len(h.h)-1]

		h.h = h.h[:len(h.h)-1]

		h.down(0)
		return out
	}

}

func (h *Heap[Item]) swap(i, j int) {
	h.h[j], h.h[i] = h.h[i], h.h[j]
}

// up decides if the child at index child needs to be moved up the heap
func (h *Heap[Item]) up(child int) {
	if child == 0 {
		return // at the top of the heap
	}

	parent := (child - 1) >> 1

	if !h.less(h.h[child], h.h[parent]) {
		return // heap order is intact
	}

	h.swap(child, parent)
	h.up(parent) // we swapped to parent, further corrections may be needed
}

// down decides if the parent at index parent needs to be moved down the heap
func (h *Heap[Item]) down(parent int) {
	lessIdx := parent
	lChild, rChild := (parent<<1)+1, (parent<<1)+2

	if lChild < len(h.h) && h.less(h.h[lChild], h.h[lessIdx]) {
		lessIdx = lChild
	}

	if rChild < len(h.h) && h.less(h.h[rChild], h.h[lessIdx]) {
		lessIdx = rChild
	}

	if lessIdx == parent {
		// don't need to move, min is at the top
		return
	}

	h.swap(lessIdx, parent)

	// further corrections could be needed
	h.down(lessIdx)
}
