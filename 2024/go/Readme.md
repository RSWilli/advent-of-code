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
go test -bench=.
```

### Results

Part 1 and 2 include the file opening and parsing.

|  Day  |  Part 1   |  Part 2   |
| :---: | :-------: | :-------: |
|   1   | 310426 ns | 299210 ns |