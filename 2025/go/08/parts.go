package day08

import (
	"aoc2025/lib/aocapi"
	"aoc2025/lib/aocparse"
	aocds "aoc2025/lib/ds"
	"aoc2025/lib/threedimensional"
	"cmp"
	"fmt"
	"maps"
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

func Part1(in string) string {
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
		for j, box2 := range junctionBoxes {
			if i == j {
				continue
			}

			h.Push(Connection{
				From: box1,
				To:   box2,
			})
		}
	}

	allCircuits := make(map[*Circuit]struct{})
	boxToCircuit := make(map[Box]*Circuit)

	for range len(junctionBoxes) / 2 {
		conn := h.Pop()

		fromCircuit, okFrom := boxToCircuit[conn.From]
		toCircuit, okTo := boxToCircuit[conn.To]

		if okFrom && okTo && fromCircuit == toCircuit {
			continue
		}

		fmt.Println("connecting:", conn.String())

		var targetCircuit *Circuit
		if !okFrom && !okTo {
			targetCircuit = &Circuit{
				Members: make(map[Box]struct{}),
			}

			allCircuits[targetCircuit] = struct{}{}
		} else if okFrom && !okTo {
			targetCircuit = fromCircuit
		} else if !okFrom && okTo {
			targetCircuit = toCircuit
		} else if okFrom && okTo {
			delete(allCircuits, toCircuit)

			maps.Copy(fromCircuit.Members, toCircuit.Members)

			targetCircuit = fromCircuit
		}

		targetCircuit.Members[conn.From] = struct{}{}
		targetCircuit.Members[conn.To] = struct{}{}

		boxToCircuit[conn.From] = targetCircuit
		boxToCircuit[conn.To] = targetCircuit

		for c := range allCircuits {
			fmt.Println("Circuit:")

			for b := range c.Members {
				fmt.Printf("%s, ", b.String())
			}
			fmt.Println()
		}
	}

	fmt.Println(allCircuits)

	circuitSizes := make([]int, 0, len(allCircuits))

	for c := range allCircuits {
		circuitSizes = append(circuitSizes, len(c.Members))
	}

	slices.SortFunc(circuitSizes, func(a, b int) int {
		return cmp.Compare(b, a)
	})

	fmt.Println(circuitSizes)

	product := 1

	for _, c := range circuitSizes[0:2] {
		product *= c
	}

	return fmt.Sprintf("%d", product)
}

func Part2(in string) string {
	panic("unimplemented")
}
