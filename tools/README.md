# puzzle-tools

A Rust CLI for authoring and checking clue sets for the seating puzzle in
`../src/`. Replaces an earlier set of Node scripts (same algorithms, much
faster - the full-grid greedy search that took a long while under Node runs
in ~20s here).

Build once with `cargo build --release` (or use `cargo run --` during
authoring); the binary is `target/debug/puzzle-tools` or
`target/release/puzzle-tools`.

## Important: unique solvability is NOT a requirement

Earlier authoring sessions burned a lot of time trying to guarantee every
clue set has exactly one solution. **Don't do that by default.** A clue set
only needs to be:

- internally consistent (satisfiable - `solve` reports at least 1 solution), and
- interesting enough for a player to reason about.

Multiple valid seatings are fine. Reach for `verify`/`author-grid`'s
uniqueness search only if a puzzle specifically calls for it - it's a much
more expensive property to chase than it looks, especially as the roster
grows (the row/column counts in `Types.elm` determine board size, e.g. 2
rows x 6 columns = 12 students).

## Data files

`data/roster.json` - the current layout, as rows front-to-back:
`{"rows": [[...names...], [...names...], ...]}` (one array per row - must
match `rowsPerBoard`/`columnsPerRow` in `../src/Types.elm`).

`data/clues.json` - the clue set for that roster, consumed by `emit-elm` to
produce `src/Puzzle.elm`'s `students`/`axioms`. Each clue is
`{"rel": "nextTo" | "leftOf" | "sameBench" | "behind" | "inRow", "a": name,
"b": name (or row number for inRow), "negated": bool}`.

## Subcommands

- `solve --roster <roster.json> --clues <clues.json> [--cap N] [--budget N]`
  Counts how many seatings satisfy the clue set (capped at `--cap`, default
  10, for speed). Prints the first solution found.

- `verify --roster <roster.json> --clues <clues.json>`
  Same as `solve` but exits non-zero unless the count is exactly 1. Only use
  this when uniqueness is actually the goal.

- `author-row --roster <roster.json> --target-remaining N --out <out.json>`
  For each row in the roster, greedily picks negative-only clues (`NOT
  nextTo` / `NOT sameBench` / `NOT leftOf`) that narrow that row's n!
  orderings down to `--target-remaining` possibilities (use `1` for a fully
  unique order - cheap up to ~8 people per row). Writes
  `{"rows": [[clues...], [clues...], ...]}`.

- `author-grid --roster <roster.json> --max-axioms N --cap N --out <out.json>`
  Greedy set-cover over the *whole* board at once (not row-by-row): at each
  step, picks whichever candidate clue shrinks the global solution count the
  most, stopping at a unique solution or `--max-axioms`. This is the
  expensive one - only reach for it if you actually want a unique solution
  and are OK with the runtime.

- `emit-elm --roster <roster.json> --clues <clues.json> [--out Puzzle.elm]`
  Renders `students`/`axioms` Elm source from a roster + clue set. Colors
  are auto-generated (evenly spaced hues); paste the output into
  `src/Puzzle.elm` (or point `--out` at it directly) and adjust colors by
  hand if you want something less rainbow-y.

## Example: the current 12-student puzzle

```
cargo run -- author-row --roster data/roster.json --target-remaining 1 --out /tmp/row_clues.json
# then hand-assemble InRow anchors (only the front row needs them - the
# rest falls out by elimination once a row's seats are exactly accounted
# for) + the row clues into a clues JSON (see data/clues.json for the
# shape), and:
cargo run -- emit-elm --roster data/roster.json --clues data/clues.json --out ../src/Puzzle.elm
```

Resizing the board (row/bench counts) is a two-step edit: change
`rowsPerBoard`/`benchesPerRow` in `../src/Types.elm` first (that's what
determines `Types.allSeats`, so it must match the roster's shape), then
rebuild the roster/clues/`Puzzle.elm` as above.
