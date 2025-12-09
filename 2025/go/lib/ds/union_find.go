package aocds

import (
	"maps"
	"slices"
)

type UnionFind[T comparable] struct {
	currentId int
	sets      map[int]map[T]struct{}
}

func NewUnionFind[T comparable](elements []T) *UnionFind[T] {
	uf := &UnionFind[T]{
		sets: make(map[int]map[T]struct{}),
	}

	for _, el := range elements {
		uf.sets[uf.currentId] = map[T]struct{}{el: {}}
		uf.currentId++
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
	sU, iU, okU := uf.Find(u)
	sV, iV, okV := uf.Find(v)

	if !okU || !okV {
		panic("set not found")
	}

	if iV == iU {
		return
	}

	maps.Copy(sU, sV)

	delete(uf.sets, iV)
}

func (uf *UnionFind[T]) UniqueSets() int {
	return len(uf.sets)
}

func (uf *UnionFind[T]) Sets() []map[T]struct{} {
	return slices.Collect(maps.Values(uf.sets))
}
