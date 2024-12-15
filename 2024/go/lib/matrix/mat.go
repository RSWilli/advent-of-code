package matrix

// M22 represents a 2x2 matrix in the following way:
// A C
// B D
type M22 struct {
	A, B, C, D int
}

func (m M22) Determinant() int {
	return m.A*m.D - m.B*m.C
}
