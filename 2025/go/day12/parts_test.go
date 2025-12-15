package day12_test

import (
	"aoc2025/day12"
	"fmt"
	"testing"
)

var test = `
0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
`

func TestParse(t *testing.T) {
	out := day12.Parse(test)

	fmt.Println(out)
}
