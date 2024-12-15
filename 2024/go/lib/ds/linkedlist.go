package aocds

import (
	"fmt"
	"iter"
)

type (
	LinkedList[T any] struct {
		Start *LinkedListEntry[T]
		End   *LinkedListEntry[T]
	}

	LinkedListEntry[T any] struct {
		prev *LinkedListEntry[T]
		next *LinkedListEntry[T]
		list *LinkedList[T]
		Item T
	}
)

func NewLinkedList[T any]() *LinkedList[T] {
	return &LinkedList[T]{}
}

func (ll *LinkedList[T]) Append(item T) {
	newitem := &LinkedListEntry[T]{
		prev: ll.End,
		next: nil,
		list: ll,
		Item: item,
	}

	if ll.End != nil {
		ll.End.next = newitem
	}

	if ll.Start == nil {
		ll.Start = newitem
	}

	ll.End = newitem
}

func (ll *LinkedList[T]) String() string {
	current := ll.Start

	str := "LinkedList { "

	for current != nil {
		str += fmt.Sprint(current.Item)

		str += " "

		current = current.next
	}

	str += "}"

	return str
}

func (lle *LinkedListEntry[T]) InsertAfter(item T) {
	if lle == nil {
		panic("insert after called on nil entry")
	}

	newitem := &LinkedListEntry[T]{
		prev: lle,
		next: lle.next,
		list: lle.list,
		Item: item,
	}

	lle.next = newitem

	if lle.list.End == lle {
		lle.list.End = newitem
	}
}

func (lle *LinkedListEntry[T]) Next() *LinkedListEntry[T] {
	return lle.next
}

func (lle *LinkedListEntry[T]) Prev() *LinkedListEntry[T] {
	return lle.prev
}

// Remove removes the entry from the list
func (lle *LinkedListEntry[T]) Remove() {
	if lle == nil {
		panic("remove called on nil entry")
	}

	if lle.prev == nil {
		lle.list.Start = lle.next
	} else {
		lle.prev.next = lle.next
	}

	if lle.next == nil {
		lle.list.End = lle.prev
	} else {
		lle.next.prev = lle.prev
	}

	lle.next = nil
	lle.prev = nil
}

func (ll *LinkedList[T]) Entries() iter.Seq2[int, T] {
	return func(yield func(int, T) bool) {
		current := ll.Start
		i := 0
		for current != nil {
			if !yield(i, current.Item) {
				return
			}

			current = current.next
			i++
		}
	}
}
