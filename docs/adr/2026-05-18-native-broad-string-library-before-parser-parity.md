# ADR: Build Native Broad String Support Before Parser Parity

Date: 2026-05-18
Status: Accepted

## Context

The compiler-in-`.mlfp` frontend seed currently proves only a bounded symbolic
lexer/parser fixture. Advancing to **Full Canonical `.mlfp` Parser Parity**
requires real source-text parsing, but parser-private string helpers would hide
the actual self-boot prerequisite: `.mlfp` programs need a public, native-capable
text substrate.

## Decision

The next parser-parity family is ordered in two tracks. First, implement a
Prelude-level **Broad String Library** with **Unicode Scalar `Char`**,
single-quoted char literals, double-quoted string literals, valid-text
boundaries, `String` as a Unicode-scalar sequence, `String`/`List Char`
conversion, plain search, explicit formatting, Unicode-default text operations,
and **Full Native String Slicing And Classification** across source checking,
`run-program`, backend, object, and native execution. Then build the
parser-owned combinator core and the full canonical `.mlfp` parser on top of
that library.

Locale-sensitive APIs and regex are excluded from this roadmap. Parser
combinators remain parser-owned `.mlfp` modules rather than a Prelude-level
library.

## Rejected Alternatives

- Keep string operations parser-private. That would make parser parity depend on
  hidden compiler-frontend helpers instead of a reusable self-boot substrate.
- Implement string operations only for `run-program`. That would allow parser
  evidence to pass in the interpreter while still failing the native compiler
  workload.
- Add only a narrow parser-useful string API. That would underfit the broader
  self-boot need for ordinary `.mlfp` text processing.
- Include locale-sensitive APIs or regex now. Those require separate semantic
  contracts and would distract from Unicode-default string behavior and plain
  parser/search needs.

## Consequences

- Full parser parity cannot be claimed until the broad string/char library is
  native-capable, not merely source-checked or interpreter-runnable.
- Native string work must preserve Unicode scalar semantics instead of treating
  strings as byte-indexed C strings.
- Parser evidence must include native execution where string operations are on
  the compiler frontend path.
- The self-boot roadmap should treat broad text support as compiler substrate,
  not as an optional standard-library convenience.
