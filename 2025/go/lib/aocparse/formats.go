package aocparse

import "strings"

// ParseTable parses a "beatifully" formatted string table
// into a 2d array
//
// the columns are separated by a single space and the cells can contain
// additional spaces:
//
//	"123 328  51 64 "
//	" 45 64  387 23 "
//	"  6 98  215 314"
//	"*   +   *   +  "
func ParseFWF(in string) [][]string {
	lines := strings.Split(in, "\n")

	if len(lines) == 0 {
		return nil
	}

	res := make([][]string, len(lines))

	colend := 0

	for len(lines[0]) > 0 {
		// find col width
		for _, line := range lines {
			for i, c := range line {
				if c == ' ' {
					break
				}

				colend = max(colend, i+1)
			}
		}

		// split words
		for i, line := range lines {
			res[i] = append(res[i], line[:colend])

			if colend+1 < len(lines[i]) {
				lines[i] = lines[i][colend+1:]
			} else {
				lines[i] = ""
			}
		}

		colend = 0
	}

	return res
}
