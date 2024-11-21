package aocinput

import (
	"fmt"
	"testing"
)

type Test struct {
	Day      int
	Test     int
	Part     AOCFunc
	Expected string
}

func (t Test) String() string {
	return fmt.Sprintf("test-%d-%d", t.Day, t.Test)
}

func RunTests(t *testing.T, tests []Test) {

	for _, test := range tests {
		t.Run(test.String(), func(t *testing.T) {
			r := Reader{
				Day:    test.Day,
				Test:   true,
				Testnr: test.Test,
			}

			out, err := test.Part(r)

			if err != nil {
				t.Fatal(err)
			}

			if out != test.Expected {
				t.Fatalf(`Did not get expected output, expected "%s", got "%s"`, test.Expected, out)
			}
		})
	}

}
