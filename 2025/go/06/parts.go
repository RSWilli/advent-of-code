package day06

import (
	"aoc2025/lib/aocapi"
	"aoc2025/lib/aocparse"
	"fmt"
	"strings"
)

func GetInput() string {
	i, err := aocapi.GetInput(2025, 6)

	if err != nil {
		panic(err)
	}

	return i
}

type Problem struct {
	Numbers   []int
	Operation string
}

func Part1(in string) string {
	lines := strings.Split(in, "\n")

	problems := make(map[int]*Problem)

	for _, l := range lines {
		words := strings.Fields(l)

		for x, word := range words {
			problem, ok := problems[x]

			if !ok {
				problem = &Problem{
					Numbers: make([]int, 0, len(lines)),
				}

				problems[x] = problem
			}

			switch word {
			case "+", "*":
				problem.Operation = word
			default:
				num := aocparse.Int(word)

				problem.Numbers = append(problem.Numbers, num)
			}
		}
	}

	sum := 0

	for _, p := range problems {
		result := p.Numbers[0]

		for _, n := range p.Numbers[1:] {
			if p.Operation == "+" {
				result += n
			} else {
				result *= n
			}
		}

		sum += result
	}

	return fmt.Sprintf("%d", sum)
}

func Part2(in string) string {
	in = strings.Trim(in, "\n")

	problems := make(map[int]*Problem)

	wordslines := aocparse.ParseFWF(in)

	for _, words := range wordslines {
		for x, word := range words {
			problem, ok := problems[x]

			if !ok {
				problem = &Problem{
					// prealloc:
					Numbers: make([]int, len(word)),
				}

				problems[x] = problem
			}

			if strings.HasPrefix(word, "+") {
				problem.Operation = "+"
				continue
			} else if strings.HasPrefix(word, "*") {
				problem.Operation = "*"
				continue
			}

			for i, c := range word {
				if c == ' ' {
					continue
				}

				digit := c - '0'

				problem.Numbers[i] = problem.Numbers[i]*10 + int(digit)
			}
		}
	}

	sum := 0

	for _, p := range problems {
		result := p.Numbers[0]

		for _, n := range p.Numbers[1:] {
			if p.Operation == "+" {
				result += n
			} else {
				result *= n
			}
		}

		sum += result
	}

	return fmt.Sprintf("%d", sum)
}
