package threedimensional_test

import (
	"aoc2025/lib/threedimensional"
	"testing"
)

func Test3dEuclid(t *testing.T) {
	type test struct {
		p1, p2 threedimensional.Position

		expect float64
	}
	cases := []test{
		{threedimensional.Position{0, 0, 0}, threedimensional.Position{0, 0, 10}, 10},
		{threedimensional.Position{0, 0, 0}, threedimensional.Position{0, 10, 0}, 10},
		{threedimensional.Position{0, 0, 0}, threedimensional.Position{10, 0, 0}, 10},
	}

	for _, test := range cases {
		if res := test.p1.EuclidDistance(test.p2); res != test.expect {
			t.Fatalf("expected %f got %f", test.expect, res)
		}
	}
}
