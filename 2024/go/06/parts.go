package day06

import (
	aocinput "aoc2024/lib/input"
	"aoc2024/lib/twodimensional"
	"bytes"
	"fmt"
	"sync"
	"sync/atomic"
)

func parse(r aocinput.Reader) (twodimensional.Position, twodimensional.Field[byte], error) {
	lines, err := r.ReadFileLines()

	if err != nil {
		return twodimensional.Position{}, nil, err
	}

	var twod twodimensional.Field[byte]

	var guard twodimensional.Position

	for y, line := range lines {
		guardIndex := bytes.IndexByte(line, '^')

		if guardIndex >= 0 {
			guard = twodimensional.Position{
				X: guardIndex,
				Y: y,
			}
		}

		twod = append(twod, bytes.Clone(line))
	}

	return guard, twod, nil
}

func Part1(r aocinput.Reader) (string, error) {
	currentGuardPosition, twod, err := parse(r)

	if err != nil {
		return "", err
	}

	width := len(twod[0])
	height := len(twod)

	currentDirection := twodimensional.DirectionUp

	visited := make(map[twodimensional.Position]struct{})

	for {
		visited[currentGuardPosition] = struct{}{}

		newGuardPosition := currentGuardPosition.Walk(currentDirection)

		if !newGuardPosition.InBounds(0, width-1, 0, height-1) {
			break
		}

		if twod.Lookup(newGuardPosition) == '#' {
			currentDirection = currentDirection.TurnRight()
		} else {
			currentGuardPosition = newGuardPosition
		}
	}

	return fmt.Sprintf("%d", len(visited)), nil
}

type positionState struct {
	p   twodimensional.Position
	dir twodimensional.Direction
}

func Part2(r aocinput.Reader) (string, error) {
	start, twod, err := parse(r)

	if err != nil {
		return "", err
	}

	currentGuardPosition := start

	width := len(twod[0])
	height := len(twod)

	currentDirection := twodimensional.DirectionUp

	visited := make(map[twodimensional.Position]struct{})

	for {
		visited[currentGuardPosition] = struct{}{}

		newGuardPosition := currentGuardPosition.Walk(currentDirection)

		if !newGuardPosition.InBounds(0, width-1, 0, height-1) {
			break
		}

		if twod.Lookup(newGuardPosition) == '#' {
			currentDirection = currentDirection.TurnRight()
		} else {
			currentGuardPosition = newGuardPosition
		}
	}

	startState := positionState{
		p:   start,
		dir: twodimensional.DirectionUp,
	}

	var loopedPaths atomic.Int64

	var wg sync.WaitGroup

	for boxPosition := range visited {
		if boxPosition == start {
			continue
		}

		wg.Add(1)

		go func() {
			if findloop(startState, twod, boxPosition) {
				loopedPaths.Add(1)
			}

			wg.Done()
		}()
	}

	wg.Wait()

	return fmt.Sprintf("%d", loopedPaths.Load()), nil
}

func findloop(startState positionState, twod twodimensional.Field[byte], box twodimensional.Position) bool {
	currentState := startState

	width := len(twod[0])
	height := len(twod)

	seen := make(map[positionState]struct{})

	for {
		newGuardPosition := currentState.p.Walk(currentState.dir)

		if !newGuardPosition.InBounds(0, width-1, 0, height-1) {
			return false
		}

		if newGuardPosition == box || twod.Lookup(newGuardPosition) == '#' {
			currentState.dir = currentState.dir.TurnRight()
		} else {
			currentState.p = newGuardPosition
		}

		if _, ok := seen[currentState]; ok {
			return true
		}

		seen[currentState] = struct{}{}
	}
}
