# BUG: Negative numbers rejected in compact record expressions

**Severity**: Medium — affects valid Elm code  
**Status**: Confirmed bug in elm-syntax parser  
**Found by**: coverage-guided edge case testing of minus/negation boundary

## Reproduction

```elm
module T exposing (..)

x = {a=-1}
```

The Elm compiler (0.19.1) accepts this and compiles it successfully.  
elm-syntax rejects it (parse failure).

## Affected patterns

| Source | elm-syntax | Elm compiler |
|--------|-----------|-------------|
| `{a=-1}` | REJECTS | accepts |
| `{a=1,b=-2}` | REJECTS | accepts |
| `{a=-1,b=-2,c=-3}` | REJECTS | accepts |
| `{a=1, b=-2}` | REJECTS | accepts |
| `{ a = -1 }` | accepts | accepts |
| `{ a = 1, b = -2 }` | accepts | accepts |

The bug occurs when there is **no space between `=` and `-`** in a record field assignment. With a space (`= -1`), it parses correctly.

## Root cause

The parser's negation detection (in `src/Elm/Parser/Expression.elm` around line 929) checks the character before `-` to decide whether it's negation or infix minus. The check recognizes `(`, `)`, `}`, `,`, `[` and space as negation-preceding characters, but does **not include `=`**. 

When the source is `a=-1`, the character before `-` is `=`, which is not in the recognized set, so the parser treats `-` as infix minus instead of negation.

## Suggested fix

Add `=` to the set of characters that trigger negation parsing in the `minusFollowedBy` / negation handler in `Expression.elm`.
