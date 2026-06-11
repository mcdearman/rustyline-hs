# rustyline-hs

A Haskell port of the public API of the Rust
[`rustyline`](https://crates.io/crates/rustyline) line-editing library.

The central type is an `Editor`, built from a `Config` and optionally
parameterised by a `Helper` that bundles a **completer**, **hinter**,
**highlighter** and **validator**. `readline` drives a Unix terminal in raw
mode with Emacs-style key bindings, and returns
`Either ReadlineError String` (rustyline's `Result<String, ReadlineError>`).

```haskell
import Rustyline

main :: IO ()
main = do
  ed <- newDefaultEditor
  let loop = do
        r <- readline ed "\xBB "
        case r of
          Left Eof         -> putStrLn "<EOF>"
          Left Interrupted -> putStrLn "<Ctrl-C>"
          Left _           -> pure ()
          Right line       -> do
            _ <- addHistoryEntry ed line
            putStrLn ("Line: " ++ line)
            loop
  loop
```

## Concept mapping (Rust → Haskell)

| rustyline (Rust) | rustyline-hs (Haskell) |
| --- | --- |
| `Editor<H>` with `&mut self` methods | `Editor h` holding `IORef`s; module-level functions as methods |
| `Config` + `Builder` | `Config` record + `set*` combinators chained with `&` |
| `Result<String, ReadlineError>` | `Either ReadlineError String` |
| `trait Completer { type Candidate; }` | `class Completer c` with type family `CandidateOf c` |
| `trait Hinter { type Hint; }` | `class Hinter h` with type family `HintOf h` |
| `trait Highlighter` | `class Highlighter h` (all-default methods) |
| `trait Validator` | `class Validator v` |
| `trait Helper` (super-trait of the four) | `class Helper h` + blanket instance |
| `()` as "no helper" | `()` with instances for all four classes; `DefaultEditor = Editor ()` |
| `Candidate` / `Pair` | `class Candidate` / `Pair` |
| `History` | `History` (backed by `Data.Sequence`) |
| `ReadlineError::{Eof, Interrupted, …}` | `ReadlineError(Eof, Interrupted, Io, …)` |

## Modules

- `Rustyline` — umbrella, re-exports the public surface (like rustyline's `lib.rs`).
- `Rustyline.Editor` — the stateful editor and `readline`.
- `Rustyline.Config` — configuration record and builder combinators.
- `Rustyline.History` — the history store (`Data.Sequence`-backed) with file load/save.
- `Rustyline.Completion` / `.Hint` / `.Highlight` / `.Validate` — the four helper traits, each with a small built-in implementation (`FilenameCompleter`, `HistoryHinter`, `MatchingBracketHighlighter`, `MatchingBracketValidator`).
- `Rustyline.Helper` — the `Helper` super-class and blanket instance.
- `Rustyline.KeyEvent` / `.Command` — keys and the edit commands they map to.
- `Rustyline.LineBuffer` — **pure**, testable editing core (insert/delete/move/kill/transpose).
- `Rustyline.Tty` — Unix termios backend, key decoding, single-line refresh.

The pure `LineBuffer` is deliberately separated from terminal I/O so the
editing logic is unit-testable without a TTY (see `test/Spec.hs`).

## Building

Designed to build with a plain GHC using only boot libraries
(`base`, `containers`, `directory`, `filepath`, `unix`) — no network fetch
required:

```sh
# library + example + tests, without cabal:
ghc -isrc -iapp -o example app/Main.hs
ghc -isrc -itest -o spec  test/Spec.hs && ./spec

# or with cabal:
cabal build
cabal run rustyline-hs-example
cabal test
```

## Scope and limitations

This is a faithful API port with a **documented subset** of behaviour:

- **Unix only** — uses `termios` via `System.Posix.Terminal`; no Windows console backend.
- **Emacs key bindings** only; Vi mode is represented in `Config` but not implemented.
- **Single visual line** refresh (linenoise-style); no multi-line wrap accounting.
- Cursor positioning assumes **1 column per character** (no wide/zero-width handling).
- No bracketed-paste, no incremental history search (Ctrl-R), no undo stack.
- When stdin is **not a TTY** (pipe/redirect), `readline` falls back to a plain
  line read so programs still work non-interactively; helper hooks
  (completion/validation) are inactive on that path.

Bindings implemented in the Emacs keymap include: Enter, Backspace, Delete,
arrows, Home/End, Tab (completion), and Ctrl-A/E/B/F/P/N/K/U/W/D/C/L/T plus
Alt-B/F/D.
