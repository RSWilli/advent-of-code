package day11_test

import (
	day11 "aoc2025/11"
	"testing"
)

var test = `
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
`

func TestPart1(t *testing.T) {
	expect := "5"
	if res := day11.Part1(test); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}

var test2 = `
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
`

func TestPart2(t *testing.T) {
	expect := "2"
	if res := day11.Part2(test2); res != expect {
		t.Fatalf("expected %s got %s", expect, res)
	}
}
