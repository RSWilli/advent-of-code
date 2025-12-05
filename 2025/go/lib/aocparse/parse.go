package aocparse

import (
	"strconv"
	"strings"
)

func Lines(in string) []string {
	return strings.Split(in, "\n")
}

func Int(in string) int {
	i, _ := strconv.Atoi(in)

	return i
}
