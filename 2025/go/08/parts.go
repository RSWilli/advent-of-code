package day08

import (
	"aoc2025/lib/aocapi"
	"aoc2025/lib/aocparse"
	aocds "aoc2025/lib/ds"
	"aoc2025/lib/threedimensional"
	"cmp"
	"fmt"
	"slices"
	"strings"
)

func GetInput() string {
	i, err := aocapi.GetInput(2025, 8)

	if err != nil {
		panic(err)
	}

	return i
}

type Box = threedimensional.Position

type Connection struct {
	From Box
	To   Box
}

func (c *Connection) String() string {
	return fmt.Sprintf("%s - %s", c.From.String(), c.To.String())
}

func (c *Connection) Len() float64 {
	return c.From.EuclidDistance(c.To)
}

type Circuit struct {
	Members map[Box]struct{}
}

func parse(in string) ([]Box, *aocds.Heap[Connection]) {
	lines := strings.Split(strings.Trim(in, "\n"), "\n")

	junctionBoxes := make([]Box, 0, len(lines))

	for _, line := range lines {
		parts := strings.Split(line, ",")

		junctionBoxes = append(junctionBoxes, Box{
			X: aocparse.Int(parts[0]),
			Y: aocparse.Int(parts[1]),
			Z: aocparse.Int(parts[2]),
		})
	}

	h := aocds.NewHeap(func(a, b Connection) bool {
		return a.Len() < b.Len()
	})

	for i, box1 := range junctionBoxes {
		for j, box2 := range junctionBoxes[i:] {
			if j == 0 {
				continue
			}

			h.Push(Connection{
				From: box1,
				To:   box2,
			})
		}
	}

	return junctionBoxes, h
}

func connectAmount(in string, amount int) string {
	junctionBoxes, h := parse(in)

	uf := aocds.NewUnionFind(junctionBoxes)

	for range amount {
		conn := h.Pop()

		uf.Union(conn.From, conn.To)
	}

	// find top 3:

	allCircuits := uf.Sets()

	circuitSizes := make([]int, 0, len(allCircuits))

	for _, c := range allCircuits {
		circuitSizes = append(circuitSizes, len(c))
	}

	slices.SortFunc(circuitSizes, func(a, b int) int {
		return cmp.Compare(b, a)
	})

	product := 1

	for _, c := range circuitSizes[0:3] {
		product *= c
	}

	return fmt.Sprintf("%d", product)
}

func Part1Test(in string) string {
	return connectAmount(in, 10)
}

func Part1(in string) string {
	return connectAmount(in, 1000)
}

func Part2(in string) string {
	junctionBoxes, h := parse(in)

	uf := aocds.NewUnionFind(junctionBoxes)

	lastDistance := 0

	for {
		conn := h.Pop()

		uf.Union(conn.From, conn.To)

		if uf.UniqueSets() == 1 {
			lastDistance = conn.From.X * conn.To.X

			break
		}
	}

	return fmt.Sprintf("%d", lastDistance)
}
