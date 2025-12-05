package day05

import (
	"aoc2025/lib/aocapi"
	"aoc2025/lib/aocparse"
	"fmt"
	"strings"
)

func GetInput() string {
	i, err := aocapi.GetInput(2025, 5)

	if err != nil {
		panic(err)
	}

	return i
}

type Range struct {
	From int
	To   int
}

func (r *Range) Contains(i int) bool {
	return r.From <= i && i <= r.To
}

func (r *Range) Overlaps(other Range) bool {
	return r.From <= other.To && other.From <= r.To
}

func (r *Range) Union(other Range) Range {
	return Range{
		From: min(r.From, other.From),
		To:   max(r.To, other.To),
	}
}

func parse(in string) ([]Range, []string) {
	in = strings.Trim(in, "\n")

	allRages, allIds, _ := strings.Cut(in, "\n\n")

	strRanges := strings.Split(allRages, "\n")

	ranges := make([]Range, 0, len(strRanges))

	for _, r := range strRanges {
		from, to, _ := strings.Cut(r, "-")

		ranges = append(ranges, Range{
			From: aocparse.Int(from),
			To:   aocparse.Int(to),
		})
	}

	ids := strings.Split(allIds, "\n")

	return ranges, ids
}

func Part1(in string) string {
	ranges, ids := parse(in)

	freshIDs := 0

	for _, id := range ids {
		num := aocparse.Int(id)

		for _, r := range ranges {
			if r.Contains(num) {
				freshIDs++
				break
			}
		}
	}

	return fmt.Sprintf("%d", freshIDs)
}

type Ranges struct {
	parts []Range
}

func (r *Ranges) Add(in Range) {
	nonoverlapping := make([]Range, 0, len(r.parts)+1)
	withOverlap := make([]Range, 0, len(r.parts))

	for _, p := range r.parts {
		if p.Overlaps(in) {
			withOverlap = append(withOverlap, p)
		} else {
			nonoverlapping = append(nonoverlapping, p)
		}
	}

	combined := in

	for _, p := range withOverlap {
		combined = combined.Union(p)
	}

	r.parts = append(nonoverlapping, combined)
}

func (r *Ranges) Size() int {
	s := 0
	for _, p := range r.parts {
		s += p.To - p.From + 1
	}

	return s
}

func Part2(in string) string {
	ranges, _ := parse(in)

	combined := Ranges{}

	for _, r := range ranges {
		combined.Add(r)
	}

	return fmt.Sprintf("%d", combined.Size())
}
