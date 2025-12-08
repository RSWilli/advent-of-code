package aocds

import (
	"maps"
	"slices"
)

type UnionFind[T comparable] struct {
	sets []map[T]struct{}
}

func NewUnionFind[T comparable](elements []T) *UnionFind[T] {
	uf := &UnionFind[T]{
		sets: make([]map[T]struct{}, 0, len(elements)),
	}

	for _, el := range elements {
		uf.sets = append(uf.sets, map[T]struct{}{el: {}})
	}

	return uf
}

func (uf *UnionFind[T]) Find(u T) (map[T]struct{}, int, bool) {
	for i, set := range uf.sets {
		if _, ok := set[u]; ok {
			return set, i, true
		}
	}

	return nil, -1, false
}

func (uf *UnionFind[T]) Union(u, v T) {
	sU, _, okU := uf.Find(u)
	sV, iV, okV := uf.Find(v)

	if !okU || !okV {
		panic("set not found")
	}

	maps.Copy(sU, sV)

	uf.sets = slices.Delete(uf.sets, iV, iV+1)
}

func (uf *UnionFind[T]) Sets() []map[T]struct{} {
	return uf.sets
}
