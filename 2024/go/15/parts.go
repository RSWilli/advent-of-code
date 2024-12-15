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

	fmt.Println(warehouse)

	return "", nil
}

func Part2(r aocinput.Reader) (string, error) {
	return "", nil
}
