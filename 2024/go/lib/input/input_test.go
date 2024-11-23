package aocinput_test

import (
	aocinput "aoc2024/lib/input"
	"bufio"
	"strings"
	"testing"
)

func TestSplitFunc(t *testing.T) {
	sep := ", "
	segs := []string{
		"foobar",
		"baz",
		"bam",
		"falala",
	}

	r := strings.NewReader(strings.Join(segs, sep))

	s := bufio.NewScanner(r)
	s.Split(aocinput.GetSeparatedBySplitFunc([]byte(sep)))

	i := 0
	for s.Scan() {
		if s.Text() != segs[i] {
			t.Fatalf(`expected: "%s", got: "%s"`, segs[i], s.Text())
		}

		i++
	}
}
