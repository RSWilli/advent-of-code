package twodimensional

import "fmt"

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

type Field [][]byte

func (f Field) Has(p Position) bool {
	width := len(f[0])
	height := len(f)

	return p.InBounds(0, width-1, 0, height-1)
}

func (f Field) Lookup(p Position) byte {
	return f[p.Y][p.X]
}

func (f Field) Set(p Position, b byte) {
	row := f[p.Y]

	row[p.X] = b
}
