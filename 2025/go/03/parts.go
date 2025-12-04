package day03

import (
	"aoc2025/lib/aocapi"
	"fmt"
	"math"
	"strings"
)

func GetInput() string {
	i, err := aocapi.GetInput(2025, 3)

	if err != nil {
		panic(err)
	}

	return i
}

// MaxBattery returns the max battery from the bank and its index
func MaxBattery(bank string) (index int, battery int) {
	max := 0
	maxI := 0

	for i, char := range bank {
		bat := int(char - '0')
		if bat > max {
			max = bat
			maxI = i
		}
	}

	return maxI, max
}

func MaximumJoltage(bank string, numBatteries int) int {
	possibleBank := bank[:len(bank)-numBatteries+1]
	maxIndex, maxBattery := MaxBattery(possibleBank)

	if numBatteries > 1 {
		restBank := bank[maxIndex+1:]

		battery := maxBattery * int(math.Pow10(numBatteries-1))

		maxBattery = battery + MaximumJoltage(restBank, numBatteries-1)
	}

	return maxBattery
}

func Part1(in string) string {
	in = strings.Trim(in, "\n")
	total := 0
	for bank := range strings.SplitSeq(in, "\n") {
		total += MaximumJoltage(bank, 2)
	}

	return fmt.Sprintf("%d", total)
}

func Part2(in string) string {
	in = strings.Trim(in, "\n")
	total := 0
	for bank := range strings.SplitSeq(in, "\n") {
		total += MaximumJoltage(bank, 12)
	}

	return fmt.Sprintf("%d", total)
}
