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
ghcid --target=aoc2023 --run=":! ghcid --target=spec --run" --warnings $@
```

Repl:

```
cabal repl
```

Format code:

```
ormolu -i ./**/*.hs
```

## Add a new day

Scaffold files:

```
DAY=Day07 ./scaffold.sh
```

Then extend `aoc2023.cabal` and `app/Main.hs` with the new day's module.

## Run a solution

e.g.

```
cabal run aoc2023 Day01 Part2
```
