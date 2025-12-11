package day10

import (
	"aoc2025/lib/aocapi"
	"aoc2025/lib/aocparse"
	aocsearch "aoc2025/lib/search"
	"fmt"
	"math/bits"
	"slices"
	"strings"
)

func GetInput() string {
	i, err := aocapi.GetInput(2025, 10)

	if err != nil {
		panic(err)
	}

	return i
}

type Lights uint

func (l Lights) String(len int) string {
	lights := ""

	for i := range len {
		if l&(1<<i) != 0 {
			lights += "#"
		} else {
			lights += "."
		}
	}

	return lights
}

type Button uint

func (btn Button) String() string {
	var toggles []string
	for i := range bits.Len(uint(btn)) {
		if btn&(1<<i) != 0 {
			toggles = append(toggles, fmt.Sprintf("%d", i))
		}
	}

	return fmt.Sprintf("(%s)", strings.Join(toggles, ","))
}

type Machine struct {
	// target bit pattern to achieve
	target Lights
	// amount of lights
	nlights int
	// parse buttons to bitflag toggles to XOR
	buttons []Button

	joltage string
}

func (m *Machine) String() string {

	var buttons []string

	for _, btn := range m.buttons {
		buttons = append(buttons, btn.String())
	}

	return fmt.Sprintf("[%s] %s %s", m.target.String(m.nlights), strings.Join(buttons, " "), m.joltage)
}

func (m *Machine) steps() int {
	path, _ := aocsearch.BFS(
		m.target,
		func(current Lights) []Lights {
			next := make([]Lights, 0, len(m.buttons))

			for _, btn := range m.buttons {
				next = append(next, current^Lights(btn))
			}

			return next
		},
		func(node Lights) bool {
			return node == 0
		},
	)

	intermediateStates := slices.Collect(path)

	// slices.Reverse(intermediateStates)

	// current := m.target

	// uniqueBtn := make(map[Button]struct{})

	// for _, light := range intermediateStates {
	// 	btn := current ^ light
	// 	uniqueBtn[Button(btn)] = struct{}{}
	// 	current = current ^ btn
	// 	fmt.Printf("pressed %s, result: %s\n", Button(btn).String(), current.String(m.nlights))
	// }

	return len(intermediateStates)
}

func ParseMachine(line string) Machine {
	parts := strings.Split(line, " ")

	target := Lights(0)

	targetStr := strings.Trim(parts[0], "[]")
	n := len(targetStr)

	for i, c := range targetStr {
		if c == '#' {
			target |= 1 << i
		}
	}

	m := Machine{
		nlights: n,
		target:  target,
		joltage: parts[len(parts)-1],
	}

	buttons := parts[1 : len(parts)-1]

	for _, btn := range buttons {
		btn = strings.Trim(btn, "()")
		toggles := strings.Split(btn, ",")

		button := Button(0)
		for _, t := range toggles {
			d := aocparse.Int(t)

			button |= 1 << d
		}

		m.buttons = append(m.buttons, button)
	}

	return m
}

func Part1(in string) string {
	lines := strings.Split(strings.Trim(in, "\n"), "\n")

	total := 0

	for _, line := range lines {
		m := ParseMachine(line)
		// fmt.Println(m.String())
		total += m.steps()
	}

	return fmt.Sprintf("%d", total)
}

func Part2(in string) string {
	panic("unimplemented")
}
