package day09

import (
	aocinput "aoc2024/lib/input"
	aocparse "aoc2024/lib/parse"
	"fmt"
	"strings"
)

type EntryType int

const (
	EntryTypeInvalid EntryType = iota
	EntryTypeFile
	EntryTypeFree
)

type DiskEntry struct {
	Type   EntryType
	Length int
	FileID int // only set when type == File
}

// Split splits the Diskentry into two parts at given length
func (de DiskEntry) Split(i int) (DiskEntry, DiskEntry) {
	if i == 0 {
		return DiskEntry{}, de
	}

	if de.Length <= i {
		return de, DiskEntry{}
	}

	beforeLen, afterLen := i, de.Length-i

	return DiskEntry{
			Type:   de.Type,
			Length: beforeLen,
			FileID: de.FileID,
		}, DiskEntry{
			Type:   de.Type,
			Length: afterLen,
			FileID: de.FileID,
		}
}

func (de DiskEntry) String() string {
	var c byte

	if de.Type == EntryTypeFile {
		c = byte(de.FileID) + '0' // probably wrong when not example input
	} else {
		c = '.'
	}

	return strings.Repeat(string(c), de.Length)
}

func parse(r aocinput.Reader) ([]DiskEntry, error) {
	p, err := r.GetParser()

	if err != nil {
		return nil, err
	}

	var id int
	currentType := EntryTypeFile
	var diskentries []DiskEntry

	for !p.AtEOF() {
		if p.Skip('\n') {
			break
		}

		d, ok := p.GetDigit()

		if !ok {
			return nil, fmt.Errorf("wrong input format")
		}

		if currentType == EntryTypeFile {
			diskentries = append(diskentries, DiskEntry{
				Type:   currentType,
				FileID: id,
				Length: aocparse.DecimalDigit(d),
			})

			currentType = EntryTypeFree
			id++
		} else {
			diskentries = append(diskentries, DiskEntry{
				Type:   currentType,
				FileID: 0,
				Length: aocparse.DecimalDigit(d),
			})

			currentType = EntryTypeFile
		}

	}
	return diskentries, nil
}

func calcCheckSum(disk []DiskEntry) int {
	offset := 0

	checksum := 0

	for _, e := range disk {
		for i := range e.Length {
			checksum += (offset + i) * e.FileID
		}

		offset += e.Length
	}

	return checksum
}

var emtpyDiskEntry DiskEntry

func Part1(r aocinput.Reader) (string, error) {
	diskentries, err := parse(r)

	if err != nil {
		return "", err
	}

	// i is the index of the current entry from the front
	i := 0

	for i < len(diskentries) {
		currentEmpty := diskentries[i]
		currentLastFile := diskentries[len(diskentries)-1]

		if currentEmpty.Type == EntryTypeFile {
			i++
			continue
		}

		if currentLastFile.Type == EntryTypeFree {
			diskentries = diskentries[0 : len(diskentries)-1]
			continue
		}

		inserted, restFile := currentLastFile.Split(currentEmpty.Length)
		overwritten, restEmpty := currentEmpty.Split(currentLastFile.Length)

		tmp := append(diskentries[0:i:i], inserted)

		if restEmpty != emtpyDiskEntry {
			// the file was shorter than the gap, so we have some gap left to fill next time
			tmp = append(tmp, restEmpty)
		}

		_ = overwritten

		if len(diskentries) > i+1 {
			tmp = append(tmp, diskentries[i+1:]...)
		}

		diskentries = tmp

		if restFile != emtpyDiskEntry {
			// the gap was shorter than the file, so we have some file left to insert next time

			diskentries[len(diskentries)-1] = restFile
		} else {
			diskentries = diskentries[0 : len(diskentries)-1]
		}

		i++
	}

	// fmt.Println(diskentries)

	return fmt.Sprintf("%d", calcCheckSum(diskentries)), nil
}

func Part2(r aocinput.Reader) (string, error) {
	return "", nil
}
