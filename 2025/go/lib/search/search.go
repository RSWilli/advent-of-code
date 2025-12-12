package aocsearch

import (
	aocds "aoc2025/lib/ds"
	"iter"
)

func BFS[Node comparable](start Node, next func(Node) []Node, found func(Node) bool) (iter.Seq[Node], bool) {
	// seen maps the next Node to the one that discovered it
	seen := make(map[Node]Node)

	todo := make([]Node, 0, 20)
	todo = append(todo, start)

	for len(todo) > 0 {
		current := todo[0]
		todo = todo[1:]

		if found(current) {
			return retraceSteps(start, current, seen), true
		}

		for _, neigh := range next(current) {
			if _, ok := seen[neigh]; !ok {
				todo = append(todo, neigh)

				seen[neigh] = current
			}
		}
	}

	return func(yield func(Node) bool) {}, false
}

func retraceSteps[Node comparable](start, last Node, segments map[Node]Node) iter.Seq[Node] {
	return func(yield func(Node) bool) {
		current := last

		for {
			previous, ok := segments[current]

			if !ok {
				return
			}

			if !yield(previous) {
				break
			}

			if previous == start {
				return
			}

			current = previous
		}
	}
}

// DFSCountPaths will run into circles
func DFSCountPaths[Node comparable](start Node, next func(Node) []Node, found func(Node) bool) int {
	if found(start) {
		return 1
	}

	paths := 0

	for _, n := range next(start) {
		paths += DFSCountPaths(n, next, found)
	}

	return paths
}

type EdgeTo[Node any] struct {
	Node Node
	Cost int
}

type pathSegment[Node any] struct {
	from         Node
	costEstimate int
	edge         EdgeTo[Node]
}

func Astar[Node comparable](start Node, next func(Node) []EdgeTo[Node], h func(a Node) int, found func(Node) bool) (iter.Seq[EdgeTo[Node]], bool) {
	todo := aocds.NewHeap(func(a, b pathSegment[Node]) bool {
		return a.costEstimate < b.costEstimate
	})

	todo.Push(pathSegment[Node]{
		edge: EdgeTo[Node]{
			Node: start,
			Cost: 0,
		},
		costEstimate: 0,
		from:         start,
	})

	done := make(map[Node]pathSegment[Node])

	for todo.Len() > 0 {
		current := todo.Pop()

		done[current.edge.Node] = current

		if found(current.edge.Node) {
			return makePath(current.edge.Node, done), true
		}

		for _, neigh := range next(current.edge.Node) {
			if _, ok := done[neigh.Node]; !ok {
				todo.Push(pathSegment[Node]{
					edge:         neigh,
					from:         current.edge.Node,
					costEstimate: neigh.Cost + h(neigh.Node),
				})
			}
		}
	}

	return func(yield func(EdgeTo[Node]) bool) {}, false
}

func Dijkstra[Node comparable](start Node, next func(Node) []EdgeTo[Node], found func(Node) bool) (iter.Seq[EdgeTo[Node]], bool) {
	return Astar(start, next, func(Node) int { return 0 }, found)
}

func makePath[Node comparable](last Node, segments map[Node]pathSegment[Node]) iter.Seq[EdgeTo[Node]] {
	return func(yield func(EdgeTo[Node]) bool) {
		current := last

		for {
			previous := segments[current]

			if previous.from == current {
				return // first segment links to itself
			}

			if !yield(previous.edge) {
				break
			}

			current = previous.from
		}
	}
}
