package itertools

import "iter"

func Map2[A, B, C, D any](iter iter.Seq2[A, B], f func(A, B) (C, D)) iter.Seq2[C, D] {
	return func(yield func(C, D) bool) {
		for a, b := range iter {
			c, d := f(a, b)

			if !yield(c, d) {
				break
			}
		}
	}
}
