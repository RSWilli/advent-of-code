package day12

import (
	"aoc2025/lib/aocapi"
	"aoc2025/lib/aocparse"
	"aoc2025/lib/twodimensional"
	"fmt"
	"strings"
)

func GetInput() string {
	i, err := aocapi.GetInput(2025, 12)

	if err != nil {
		panic(err)
	}

	return i
}

type Piece twodimensional.Field[byte]

// Parts returns the amount of piece parts that are filled in, aka '#'
func (p Piece) Parts() int {
	total := 0

	for _, r := range p {
		for _, c := range r {
			if c == '#' {
				total++
			}
		}
	}

	return total
}

type Region struct {
	Width  int
	Height int

	PiecesToFit []int
}

func (r Region) String() string {
	return fmt.Sprintf("%dx%d: %v", r.Width, r.Height, r.PiecesToFit)
}

func (r *Region) TriviallySolvable() bool {
	totalPieceArea := 0

	for _, pieceCount := range r.PiecesToFit {
		totalPieceArea += pieceCount * pieceWidth * pieceHeight
	}

	return r.Width*r.Height > totalPieceArea
}

func (r *Region) TriviallyUnsolvable(pieceSizes []int) bool {
	totalPieceArea := 0

	for i, pieceCount := range r.PiecesToFit {
		totalPieceArea += pieceCount * pieceSizes[i]
	}

	return r.Width*r.Height < totalPieceArea
}

type Input struct {
	Pieces  []Piece
	Regions []Region
}

const pieceWidth = 3
const pieceHeight = 3

func Parse(in string) Input {
	parts := strings.Split(strings.Trim(in, "\n"), "\n\n")

	pieces := parts[0 : len(parts)-1]
	allregions := parts[len(parts)-1]

	regions := strings.Split(allregions, "\n")

	parsed := Input{
		Pieces:  make([]Piece, 0, len(pieces)),
		Regions: make([]Region, 0, len(regions)),
	}

	for _, pieceStr := range pieces {
		// skip the index in the first line:
		f := twodimensional.NewFieldFromLines(pieceStr[3:])

		if f.Width() != pieceWidth || f.Height() != pieceHeight {
			panic("piece not correct size")
		}

		parsed.Pieces = append(parsed.Pieces, Piece(f))
	}

	for _, regionStr := range regions {
		size, pieces, _ := strings.Cut(regionStr, ": ")

		widthStr, heightStr, _ := strings.Cut(size, "x")

		piecesStrs := strings.Split(pieces, " ")

		region := Region{
			Width:       aocparse.Int(widthStr),
			Height:      aocparse.Int(heightStr),
			PiecesToFit: make([]int, 0, len(piecesStrs)),
		}

		for _, s := range piecesStrs {
			region.PiecesToFit = append(region.PiecesToFit, aocparse.Int(s))
		}

		parsed.Regions = append(parsed.Regions, region)
	}

	return parsed
}

func Part1(in string) string {
	input := Parse(in)

	pieceSizes := make([]int, 0, len(input.Pieces))

	for _, p := range input.Pieces {
		pieceSizes = append(pieceSizes, p.Parts())
	}

	solvable := 0

	for _, region := range input.Regions {
		switch {
		case region.TriviallySolvable():
			solvable++
		case region.TriviallyUnsolvable(pieceSizes):

		default:
			fmt.Printf("check out this region: %s\n", region.String())
		}
	}

	return fmt.Sprintf("%d", solvable)
}

func Part2(in string) string {
	panic("unimplemented")
}
