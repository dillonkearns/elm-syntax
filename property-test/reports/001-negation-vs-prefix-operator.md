# Difference: Negation vs Prefix Operator on non-numeric expressions

**Severity**: Low (only affects code that wouldn't type-check)
**Found by**: seed 100, Test5

## Source

```elm
value : Result () Int
value =
    count -()
```

## Difference

| Parser | Representation of `-()` |
|--------|------------------------|
| elm-syntax | `Negation UnitExpr` |
| elm-format (Elm compiler) | `Application [PrefixOperator "-", UnitExpr]` |

Both parsers agree this is an argument to `count` (function application). They disagree on how to represent the unary `-` applied to `()`:

- **elm-syntax**: Uses `Negation`, which is the dedicated AST node for unary minus
- **elm-format/Elm compiler**: Uses `Application [PrefixOperator "-", ...]`, treating `-` as a prefix operator being applied

## Impact

This only matters for expressions that wouldn't type-check anyway — you can't negate `()`. For valid Elm programs (where `-` is applied to numeric values), both parsers should agree. This is a cosmetic AST difference for invalid code.

## Related patterns

The same difference likely occurs for any `-(non-numeric-expr)`, e.g.:
- `-"hello"`
- `-(foo, bar)`
- `-True`
