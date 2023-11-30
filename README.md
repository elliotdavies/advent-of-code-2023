# Advent of Code 2023

Solutions to [AOC 2023](https://adventofcode.com/2023).

## Development

Uses GHC installed via `ghcup`.

Install dependencies:

```
cabal install
```

Watch for lib changes:

```
ghcid
```

Watch for lib and test changes, and re-run tests:

```
ghcid --target=aoc2023 --run=":! ghcid --target=spec --run"
```

Run a solution:

```
cabal run aoc2023 Day01 Part2
```
