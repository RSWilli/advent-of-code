package day07

import (
	aocinput "aoc2024/lib/input"
	"fmt"
	"math/big"
	"math/bits"
	"slices"
)

type equation struct {
	result uint
	nums   []uint
}

func parse(r aocinput.Reader) ([]equation, error) {
	parser, err := r.GetParser()

	if err != nil {
		return nil, err
	}

	var equations []equation

	for !parser.AtEOF() {
		eq := equation{}
		res, ok := parser.GetDecimal()

		if !ok {
			return nil, fmt.Errorf("wrong input format, exptected number")
		}

		eq.result = uint(res)

		if !parser.Skip(':') {
			return nil, fmt.Errorf("wrong input format, exptected \":\"")
		}

		for {
			if !parser.Skip(' ') {
				break
			}

			num, ok := parser.GetDecimal()

			if !ok {
				return nil, fmt.Errorf("wrong input format, exptected number")
			}

			eq.nums = append(eq.nums, uint(num))
		}

		if !parser.Skip('\n') {
			break
		}

		equations = append(equations, eq)
	}
	return equations, nil

}

func Concat(a, b *big.Int) *big.Int {
	f := big.NewInt(1)

	ten := big.NewInt(10)

	for f.Cmp(b) <= 0 {
		f.Mul(f, ten)
	}

	f.Mul(a, f)

	return f.Add(f, b)
}

func Part1(r aocinput.Reader) (string, error) {
	equations, err := parse(r)

	if err != nil {
		return "", err
	}

	var sum uint = 0

	for _, equation := range equations {
		possibleResults := []uint{
			equation.nums[0] + equation.nums[1],
			equation.nums[0] * equation.nums[1],
		}

		result := equation.result

		if len(equation.nums) > 2 {
			for _, a := range equation.nums[2:] {
				var lastresults []uint
				for _, r := range possibleResults {

					if r+a <= result {
						lastresults = append(lastresults, r+a)
					}

					hi, lo := bits.Mul(r, a)

					if hi == 0 && lo <= result {
						lastresults = append(lastresults, r*a)
					}
				}

				possibleResults = lastresults
			}
		}

		if slices.Contains(possibleResults, result) {
			sum += result
		}
	}

	return fmt.Sprintf("%d", sum), nil
}

func Part2(r aocinput.Reader) (string, error) {
	equations, err := parse(r)

	if err != nil {
		return "", err
	}

	sum := big.NewInt(0)

	for _, equation := range equations {
		possibleResults := []*big.Int{
			big.NewInt(int64(equation.nums[0] + equation.nums[1])),
			big.NewInt(int64(equation.nums[0] * equation.nums[1])),
			Concat(big.NewInt(int64(equation.nums[0])), big.NewInt(int64(equation.nums[1]))),
		}

		result := big.NewInt(int64(equation.result))

		if len(equation.nums) > 2 {
			for _, smallA := range equation.nums[2:] {
				var lastresults []*big.Int

				a := big.NewInt(int64(smallA))

				for _, r := range possibleResults {

					addres := big.NewInt(0)
					mulres := big.NewInt(0)
					concatres := Concat(r, a)

					addres.Add(r, a)
					mulres.Mul(r, a)

					if addres.Cmp(result) < 1 {
						lastresults = append(lastresults, addres)
					}

					if mulres.Cmp(result) < 1 {
						lastresults = append(lastresults, mulres)
					}

					if concatres.Cmp(result) < 1 {
						lastresults = append(lastresults, concatres)
					}
				}

				possibleResults = lastresults
			}
		}

		if slices.ContainsFunc(possibleResults, func(a *big.Int) bool {
			return a.Cmp(result) == 0
		}) {
			sum.Add(sum, result)
		}
	}

	return sum.String(), nil
}
