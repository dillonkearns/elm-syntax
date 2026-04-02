# Difference: Negation node vs negative literal value

**Severity**: Low (semantically equivalent)
**Found by**: seed 100, Test16

## Source

```elm
baz =
    ( .c, -479 )

-- and later:

    (-44)
```

## Difference

| Parser | Representation of `-479` | Representation of `(-44)` |
|--------|-------------------------|--------------------------|
| elm-syntax | `Negation (Integer 479)` | `Negation (Integer 44)` |
| elm-format (Elm compiler) | `IntLiteral -479` | `IntLiteral -44` |

elm-syntax always parses `-N` as `Negation (Integer N)`, while elm-format folds the negation into the literal value.

## Impact

Semantically equivalent — `-479` and `Negation 479` mean the same thing. This is a representation choice, not a bug. elm-format merges the sign into the literal for simplicity, while elm-syntax preserves the syntactic structure.

## Normalization approach

To compare these as equal, the canonical form should either:
1. Fold `Negation (CInt n)` → `CInt (-n)` and `Negation (CFloat f)` → `CFloat (-f)` (match elm-format), or
2. Split negative literals back into `Negation` (match elm-syntax)

Option 1 is simpler and more natural.
