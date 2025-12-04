package twodimensional

import (
	"aoc2025/lib/aocmath"
	"bytes"
	"fmt"
)

type Position struct {
	X int
	Y int
}

type Direction int

const (
	DirectionUp Direction = iota
	DirectionRight
	DirectionDown
	DirectionLeft
)

func (d Direction) TurnLeft() Direction {
	nd := (d - 1)

	if nd < 0 {
		nd = nd + 4
	}

	return nd
}

func (d Direction) TurnRight() Direction {
	nd := d + 1

	if nd > 3 {
		nd = nd - 4
	}

	return nd
}

func (p Position) InBounds(xmin, xmax, ymin, ymax int) bool {
	return xmin <= p.X && p.X <= xmax &&
		ymin <= p.Y && p.Y <= ymax
}

func (p Position) Add(v Vector) Position {
	return Position{
		X: p.X + v.X,
		Y: p.Y + v.Y,
	}
}

func (p Position) Distance(to Position) Vector {
	return Vector{
		X: to.X - p.X,
		Y: to.Y - p.Y,
	}
}

func (p Position) Neighbors() []Position {
	return []Position{
		{X: p.X - 1, Y: p.Y},
		{X: p.X + 1, Y: p.Y},
		{X: p.X, Y: p.Y - 1},
		{X: p.X, Y: p.Y + 1},
	}
}

func (p Position) NeighborsDiag() []Position {
	return []Position{
		{X: p.X - 1, Y: p.Y},
		{X: p.X + 1, Y: p.Y},
		{X: p.X, Y: p.Y - 1},
		{X: p.X, Y: p.Y + 1},

		{X: p.X - 1, Y: p.Y - 1},
		{X: p.X + 1, Y: p.Y - 1},
		{X: p.X - 1, Y: p.Y + 1},
		{X: p.X + 1, Y: p.Y + 1},
	}
}

func (p Position) Walk(d Direction) Position {
	switch d {
	case DirectionDown:
		return Position{X: p.X, Y: p.Y + 1}
	case DirectionLeft:
		return Position{X: p.X - 1, Y: p.Y}
	case DirectionRight:
		return Position{X: p.X + 1, Y: p.Y}
	case DirectionUp:
		return Position{X: p.X, Y: p.Y - 1}
	default:
		panic(fmt.Sprintf("unexpected twodimensional.Direction: %#v", d))
	}
}

type Field[T comparable] [][]T

func NewFieldFromLines(in string) Field[byte] {
	inB := []byte(in)
	return Field[byte](bytes.Split(bytes.Trim(inB, "\n"), []byte("\n")))
}

func (f Field[T]) Has(p Position) bool {
	width := len(f[0])
	height := len(f)

	return p.InBounds(0, width-1, 0, height-1)
}

func (f Field[T]) Lookup(p Position) T {
	return f[p.Y][p.X]
}

func (f Field[T]) Set(p Position, b T) {
	row := f[p.Y]

	row[p.X] = b
}

func (f Field[T]) Find(b T) (Position, bool) {
	for y, line := range f {
		for x, c := range line {
			if b == c {
				return Position{X: x, Y: y}, true
			}
		}
	}

	return Position{}, false
}

func PrintField(f Field[byte]) {
	for _, line := range f {
		for _, c := range line {
			fmt.Print(string(c))
		}
		fmt.Println()
	}
}

type Vector struct {
	X int
	Y int
}

func (v Vector) Negate() Vector {
	return Vector{
		X: -v.X,
		Y: -v.Y,
	}
}

func (p Vector) Add(v Vector) Vector {
	return Vector{
		X: p.X + v.X,
		Y: p.Y + v.Y,
	}
}

func (v Vector) Normalize() Vector {
	gcd := aocmath.Gcd(v.X, v.Y)

	return Vector{
		X: v.X / gcd,
		Y: v.Y / gcd,
	}
}
