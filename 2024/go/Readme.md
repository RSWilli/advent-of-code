# Advent of code 2024 in go

## Run

Run the whole day, expects the inputs to be in the `inputs` folder.

```bash
go run . -day=1
```

Run against a test file, expects a `day01_1.txt` file in the `tests` folder.

```bash
go run . -day=1 -test -testnr=1
```

## Run tests

```bash
go test ./...
```

## Benchmark

```bash
go test -bench=. ./...
```

### Results

Part 1 and 2 include the file opening and parsing.

|  Day  |    Part 1     |    Part 2    |
| :---: | :-----------: | :----------: |
|   1   |   310426 ns   |  299210 ns   |
|   2   |   475071 ns   |  1168939 ns  |
|   3   |   646889 ns   |  633944 ns   |
|   4   |   786766 ns   |  283831 ns   |
|   5   |   830960 ns   |  1234410 ns  |
|   6   |  1308708 ns   | 722445181 ns |
|   7   |  5039154 ns   | 95803153 ns  |
|   8   |   199527 ns   |  625550 ns   |
|   9   | 1855302735 ns |      ?       |
|  10   |  1007225 ns   |  732661 ns   |
|  11   |   877969 ns   | 33971106 ns  |