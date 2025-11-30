package aocapi

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"time"
)

// GetInput returns the input for the given day and year.
// curl "https://adventofcode.com/${YEAR}/day/${DATE}/input" -H "Cookie: session=$SESSION"
//
// see also https://www.reddit.com/r/adventofcode/s/jEK7Bgjpp8
func GetInput(year, date int) (string, error) {
	url := fmt.Sprintf("https://adventofcode.com/%d/day/%d/input", year, date)

	req, err := http.NewRequest(http.MethodGet, url, nil)

	if err != nil {
		return "", fmt.Errorf("could not create request: %w", err)
	}

	req.AddCookie(&http.Cookie{
		Name:  "session",
		Value: os.Getenv("SESSION"),
	})

	req.Header.Add("User-Agent", "github.com/RSWilli/advent-of-code")

	res, err := http.DefaultClient.Do(req)

	if err != nil {
		return "", fmt.Errorf("error fetching input: %w", err)
	}
	defer res.Body.Close()

	if res.StatusCode >= 300 {
		data, _ := io.ReadAll(res.Body)

		return "", fmt.Errorf("got unexpected status code from input request: %d, %s", res.StatusCode, string(data))
	}

	data, err := io.ReadAll(res.Body)

	if err != nil {
		return "", fmt.Errorf("error while reading body: %w", err)
	}

	return string(data), nil
}

// GetToday returns today's input
func GetToday() (string, error) {
	time := time.Now()

	return GetInput(time.Year(), time.Day())
}
