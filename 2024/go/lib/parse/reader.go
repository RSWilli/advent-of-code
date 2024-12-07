package aocparse

import (
	"bufio"
	"bytes"
	"io"
)

type Reader struct {
	r *bufio.Reader
}

func NewReader(rd io.Reader) *Reader {
	return &Reader{
		r: bufio.NewReader(rd),
	}
}

func (rd *Reader) GetDecimal() (int, bool) {
	found := false

	num := 0

	for {
		d, ok := rd.GetDigit()

		if !ok {
			break
		}

		found = true

		num *= 10
		num += DecimalDigit(d)
	}

	if !found {
		return 0, false
	}

	return num, true
}

// func (rd *Reader) GetBigDecimal() (*big.Int, bool) {
// 	found := false

// 	ten := big.NewInt(10)
// 	num := big.NewInt(0)

// 	for {
// 		d, ok := rd.GetDigit()

// 		if !ok {
// 			break
// 		}

// 		found = true

// 		num.Mul(num, ten)
// 		num.Add(num, big.NewInt(int64(DecimalDigit(d))))
// 	}

// 	if !found {
// 		return nil, false
// 	}

// 	return num, true
// }

func (rd *Reader) Advance() (byte, bool) {
	b, err := rd.r.ReadByte()

	if err != nil {
		return 0, false
	}

	return b, true
}

func (rd *Reader) GetDigit() (byte, bool) {
	return rd.GetASCIIRange('0', '9')
}

func (rd *Reader) AtEOF() bool {
	d, _ := rd.r.Peek(1)

	return len(d) == 0
}

func (rd *Reader) Skip(b byte) bool {
	d, err := rd.r.ReadByte()

	if err != nil {
		return false
	}

	if d != b {
		err = rd.r.UnreadByte()

		if err != nil {
			panic(err)
		}

		return false
	}

	return true
}

func (rd *Reader) GetASCIIRange(min, max byte) (byte, bool) {
	b, err := rd.r.ReadByte()

	if err != nil {
		return 0, false
	}

	if min <= b && b <= max {
		return b, true
	}

	err = rd.r.UnreadByte()

	if err != nil {
		panic(err)
	}

	return 0, false
}

func (rd *Reader) Preview(n int) []byte {
	d, _ := rd.r.Peek(n)
	return d
}

func (rd *Reader) SkipPrefix(prefix []byte) bool {

	data, err := rd.r.Peek(len(prefix))

	if err != nil {
		return false
	}

	ok := bytes.HasPrefix(data, prefix)

	if !ok {
		return false
	}

	rd.r.Discard(len(prefix))

	return true
}
