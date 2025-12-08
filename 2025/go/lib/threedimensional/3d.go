package threedimensional

import (
	"fmt"
	"math"
)

type Position struct {
	X int
	Y int
	Z int
}

// https://en.wikipedia.org/wiki/Euclidean_distance#Higher_dimensions
func (p *Position) EuclidDistance(other Position) float64 {
	return math.Sqrt(
		math.Pow(float64(p.X)-float64(other.X), 2) +
			math.Pow(float64(p.Y)-float64(other.Y), 2) +
			math.Pow(float64(p.Z)-float64(other.Z), 2),
	)
}

func (p *Position) String() string {
	return fmt.Sprintf("%d,%d,%d", p.X, p.Y, p.Z)
}
