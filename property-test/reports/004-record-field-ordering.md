# Difference: Record expression field ordering

**Severity**: Low (semantically equivalent, but notable)
**Found by**: seed 100, Test18

## Source

```elm
state =
    { val = -982, baz = "test 123", model = .head }
```

## Difference

| Parser | Field order |
|--------|-----------|
| elm-syntax | `val, baz, model` (source order) |
| elm-format (Elm compiler) | `baz, model, val` (alphabetical) |

elm-format sorts record expression fields alphabetically in its JSON output, while elm-syntax preserves the order as written in source.

## Impact

Semantically equivalent — record field order doesn't matter in Elm. But this causes diff noise in AST comparison.

## Normalization approach

Sort record expression fields by name in the canonical form before comparison.
