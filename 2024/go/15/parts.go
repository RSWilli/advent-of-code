package day15

import (
	aocinput "aoc2024/lib/input"
	"aoc2024/lib/twodimensional"
	"fmt"
)

type warehouse struct {
	m            twodimensional.Field[byte]
	instructions []twodimensional.Direction
}

const empty = '.'
const wall = '#'
const singleBox = 'O'
const robot = '@'

const doubleBoxLeft = '['
const doubleBoxRight = ']'

func (w *warehouse) canMove(pos twodimensional.Position, dir twodimensional.Direction) bool {
	newpos := pos.Walk(dir)

	switch w.m.Lookup(newpos) {
	case empty:
		return true
	case wall:
		return false
	case singleBox:
		return w.canMove(newpos, dir)
	case doubleBoxLeft:
		if dir == twodimensional.DirectionUp || dir == twodimensional.DirectionDown {
			rightBoxPos := twodimensional.Position{X: newpos.X + 1, Y: newpos.Y}
			return w.canMove(newpos, dir) && w.canMove(rightBoxPos, dir)
		}

		return w.canMove(newpos, dir)
	case doubleBoxRight:
		if dir == twodimensional.DirectionUp || dir == twodimensional.DirectionDown {
			leftBoxPos := twodimensional.Position{X: newpos.X - 1, Y: newpos.Y}
			return w.canMove(leftBoxPos, dir) && w.canMove(newpos, dir)
		}

		return w.canMove(newpos, dir)
	default:
		panic("unexpected symbol")
	}
}

// moveAll does not check if the target can be moved, must be called after canMove
func (w *warehouse) moveAll(pos twodimensional.Position, dir twodimensional.Direction) twodimensional.Position {
	newpos := pos.Walk(dir)
	object := w.m.Lookup(pos)

	switch w.m.Lookup(newpos) {
	case empty:
		// nothing more to do except switching the tiles
	case singleBox:
		// continue moving
		w.moveAll(newpos, dir)
	case doubleBoxLeft:
		if dir == twodimensional.DirectionUp || dir == twodimensional.DirectionDown {
			rightBoxPos := twodimensional.Position{X: newpos.X + 1, Y: newpos.Y}

			w.moveAll(newpos, dir)
			w.moveAll(rightBoxPos, dir)
		} else {
			w.moveAll(newpos, dir)
		}
	case doubleBoxRight:
		if dir == twodimensional.DirectionUp || dir == twodimensional.DirectionDown {
			leftBoxPos := twodimensional.Position{X: newpos.X - 1, Y: newpos.Y}

			w.moveAll(newpos, dir)
			w.moveAll(leftBoxPos, dir)
		} else {
			w.moveAll(newpos, dir)
		}
	default:
		panic("unexpected symbol")
	}

	// move the current
	w.m.Set(pos, empty)
	w.m.Set(newpos, object)

	return newpos
}

func (w *warehouse) step(current twodimensional.Position) twodimensional.Position {
	dir := w.instructions[0]
	w.instructions = w.instructions[1:]

	if w.canMove(current, dir) {
		return w.moveAll(current, dir)
	}

	return current
}

func parse(r aocinput.Reader) (warehouse, error) {
	p, err := r.GetParser()

	if err != nil {
		return warehouse{}, err
	}

	w := warehouse{
		m:            make(twodimensional.Field[byte], 0, 100),
		instructions: make([]twodimensional.Direction, 0, 500),
	}

	for {
		row, ok := p.ReadUntil('\n')

		if !ok {
			return warehouse{}, aocinput.ErrWrongInput
		}

		r := row[:len(row)-1]

		if len(r) == 0 {
			break // end of map
		}

		w.m = append(w.m, r)
	}

	for !p.AtEOF() {
		b, ok := p.ReadAny('<', '>', '^', 'v', '\n')

		if !ok {
			return warehouse{}, aocinput.ErrWrongInput
		}

		switch b {
		case '<':
			w.instructions = append(w.instructions, twodimensional.DirectionLeft)
		case '>':
			w.instructions = append(w.instructions, twodimensional.DirectionRight)
		case '^':
			w.instructions = append(w.instructions, twodimensional.DirectionUp)
		case 'v':
			w.instructions = append(w.instructions, twodimensional.DirectionDown)
		case '\n':
			continue
		default:
			panic("unreachable")
		}
	}

	return w, nil
}

func Part1(r aocinput.Reader) (string, error) {
	warehouse, err := parse(r)

	if err != nil {
		return "", err
	}

	current, ok := warehouse.m.Find(robot)

	if !ok {
		return "", fmt.Errorf("could not find robot")
	}

	for len(warehouse.instructions) > 0 {
		current = warehouse.step(current)
	}

	gpsSum := 0

	for y, line := range warehouse.m {
		for x, c := range line {
			if c == singleBox {
				gpsSum += 100*y + x
			}
		}
	}

	return fmt.Sprintf("%d", gpsSum), nil
}

func stretchWarehouse(w warehouse) warehouse {
	stretched := warehouse{
		m:            make(twodimensional.Field[byte], 0, len(w.m)),
		instructions: w.instructions,
	}

	for _, line := range w.m {
		stretchedLine := make([]byte, 0, len(line)*2)

		for _, c := range line {
			switch c {
			case wall:
				stretchedLine = append(stretchedLine, wall, wall)
			case robot:
				stretchedLine = append(stretchedLine, robot, empty)
			case empty:
				stretchedLine = append(stretchedLine, empty, empty)
			case singleBox:
				stretchedLine = append(stretchedLine, doubleBoxLeft, doubleBoxRight)
			default:
				panic("unknown symbol")
			}
		}

		stretched.m = append(stretched.m, stretchedLine)
	}

	return stretched
}

func Part2(r aocinput.Reader) (string, error) {
	warehouse, err := parse(r)

	if err != nil {
		return "", err
	}

	warehouse = stretchWarehouse(warehouse)

	current, ok := warehouse.m.Find(robot)

	if !ok {
		return "", fmt.Errorf("could not find robot")
	}

	for len(warehouse.instructions) > 0 {
		current = warehouse.step(current)

		// twodimensional.PrintField(warehouse.m)
		// fmt.Println()
	}

	gpsSum := 0

	for y, line := range warehouse.m {
		for x, c := range line {
			if c == doubleBoxLeft {
				gpsSum += 100*y + x
			}
		}
	}

	return fmt.Sprintf("%d", gpsSum), nil
}
