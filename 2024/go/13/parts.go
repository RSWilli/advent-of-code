package day13

import (
	aocinput "aoc2024/lib/input"
	"aoc2024/lib/matrix"
	aocparse "aoc2024/lib/parse"
	"fmt"
)

// essentially:
//
// ax * a + bx * b = px
// ay * a + by * b = py
type arcade struct {
	buttonAX int
	buttonAY int

	buttonBX int
	buttonBY int

	prizeX int
	prizeY int
}

// getsPrize checks whether a and b button pushes get the prize
func (arc arcade) getsPrize(a, b int) bool {
	return arc.buttonAX*a+arc.buttonBX*b == arc.prizeX &&
		arc.buttonAY*a+arc.buttonBY*b == arc.prizeY
}

func (arc arcade) requiredPushes() (a int, b int, solvable bool) {
	// cramers rule:
	coeff := matrix.M22{
		A: arc.buttonAX, C: arc.buttonBX,
		B: arc.buttonAY, D: arc.buttonBY,
	}

	det := coeff.Determinant()

	if det == 0 {
		return 0, 0, false
	}

	coeffA := matrix.M22{
		A: arc.prizeX, C: arc.buttonBX,
		B: arc.prizeY, D: arc.buttonBY,
	}

	coeffB := matrix.M22{
		A: arc.buttonAX, C: arc.prizeX,
		B: arc.buttonAY, D: arc.prizeY,
	}

	return coeffA.Determinant() / det, coeffB.Determinant() / det, true
}

func parseButton(p *aocparse.Reader) (int, int, bool) {
	if !p.SkipPrefix([]byte("Button ")) {
		return 0, 0, false
	}

	if _, ok := p.GetASCIIRange('A', 'B'); !ok {
		return 0, 0, false
	}

	if !p.SkipPrefix([]byte(": X")) {
		return 0, 0, false
	}

	x, ok := p.GetSignedDecimal()

	if !ok {
		return 0, 0, false
	}

	if !p.SkipPrefix([]byte(", Y")) {
		return 0, 0, false
	}

	y, ok := p.GetSignedDecimal()

	if !ok {
		return 0, 0, false
	}

	if !p.Skip('\n') {
		return 0, 0, false
	}

	return x, y, true
}

func parsePrize(p *aocparse.Reader) (int, int, bool) {
	if !p.SkipPrefix([]byte("Prize: X=")) {
		return 0, 0, false
	}

	x, ok := p.GetSignedDecimal()

	if !ok {
		return 0, 0, false
	}

	if !p.SkipPrefix([]byte(", Y=")) {
		return 0, 0, false
	}

	y, ok := p.GetSignedDecimal()

	if !ok {
		return 0, 0, false
	}

	if !p.Skip('\n') {
		return 0, 0, false
	}

	return x, y, true
}

func parse(r aocinput.Reader) ([]arcade, error) {
	p, err := r.GetParser()

	if err != nil {
		return nil, err
	}

	arcades := make([]arcade, 0, 100)

	for !p.AtEOF() {
		var ok bool
		var a arcade

		a.buttonAX, a.buttonAY, ok = parseButton(p)

		if !ok {
			return nil, aocinput.ErrWrongInput
		}

		a.buttonBX, a.buttonBY, ok = parseButton(p)

		if !ok {
			return nil, aocinput.ErrWrongInput
		}

		a.prizeX, a.prizeY, ok = parsePrize(p)

		if !ok {
			return nil, aocinput.ErrWrongInput
		}

		arcades = append(arcades, a)

		if !p.Skip('\n') {
			break
		}
	}

	return arcades, nil
}

const tokensPerAPush = 3
const tokensPerBPush = 1

const limit = 100

func Part1(r aocinput.Reader) (string, error) {
	arcades, err := parse(r)

	if err != nil {
		return "", err
	}

	totalTokens := 0

	for _, a := range arcades {
		apushes, bpushes, reachable := a.requiredPushes()

		if reachable &&
			0 <= apushes && apushes <= limit &&
			0 <= bpushes && bpushes <= limit {

			if !a.getsPrize(apushes, bpushes) {
				// the solution could be fractional
				continue
			}
			totalTokens += tokensPerAPush*apushes + tokensPerBPush*bpushes
		}
	}

	return fmt.Sprintf("%d", totalTokens), nil
}

const offsetPrize = 10000000000000

func Part2(r aocinput.Reader) (string, error) {
	arcades, err := parse(r)

	if err != nil {
		return "", err
	}

	totalTokens := 0

	for _, a := range arcades {
		a.prizeX += offsetPrize
		a.prizeY += offsetPrize

		apushes, bpushes, reachable := a.requiredPushes()

		if reachable &&
			0 <= apushes &&
			0 <= bpushes {

			if !a.getsPrize(apushes, bpushes) {
				// the solution could be fractional
				continue
			}
			totalTokens += tokensPerAPush*apushes + tokensPerBPush*bpushes
		}
	}

	return fmt.Sprintf("%d", totalTokens), nil
}
