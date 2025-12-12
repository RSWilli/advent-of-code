package day11

import (
	"aoc2025/lib/aocapi"
	aocsearch "aoc2025/lib/search"
	"fmt"
	"strings"
)

func GetInput() string {
	i, err := aocapi.GetToday()

	if err != nil {
		panic(err)
	}

	return i
}

func Part1(in string) string {
	lines := strings.Split(strings.Trim(in, "\n"), "\n")

	graph := make(map[string][]string)

	for _, line := range lines {
		node, connections, _ := strings.Cut(line, ": ")

		for conn := range strings.SplitSeq(connections, " ") {
			graph[node] = append(graph[node], conn)
		}
	}

	paths := aocsearch.DFSCountPaths(
		"you",
		func(node string) []string {
			return graph[node]
		},
		func(node string) bool {
			return node == "out"
		},
	)

	return fmt.Sprintf("%d", paths)
}

func Part2(in string) string {
	lines := strings.Split(strings.Trim(in, "\n"), "\n")

	graph := make(map[string][]string)

	for _, line := range lines {
		node, connections, _ := strings.Cut(line, ": ")

		for conn := range strings.SplitSeq(connections, " ") {
			graph[node] = append(graph[node], conn)
		}
	}

	panic("unimplemented")

	return fmt.Sprintf("%d", 0)
}
