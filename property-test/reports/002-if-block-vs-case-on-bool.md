# Difference: if/then/else vs case True/False desugaring

**Severity**: Medium (structural AST difference for valid syntax)
**Found by**: seed 100, Test15 and Test16

## Source

```elm
val acc tail =
    if "test 123" then "foo" else 213.6 - ( (), .msg )
```

and

```elm
n =
    if .state then () else 54
```

## Difference

| Parser | Representation |
|--------|---------------|
| elm-syntax | `IfBlock condition thenBranch elseBranch` |
| elm-format (Elm compiler) | `CaseExpression subject [("True", thenBranch), ("False", elseBranch)]` |

elm-format desugars `if/then/else` into a `case` expression matching on `True`/`False`, while elm-syntax preserves the `if` block as-is.

## Impact

This is a **significant structural difference** — elm-format's Elm compiler parser converts `if` expressions into case expressions during parsing, while elm-syntax preserves the source syntax. Any AST comparison tool needs to account for this.

Both representations are semantically equivalent for valid Elm code. The elm compiler likely does this desugaring early to simplify later compilation stages.

## Normalization approach

To compare these as equal, the canonical form should either:
1. Normalize `IfBlock` → `CaseExpression` (match elm-format), or
2. Detect `CaseExpression` on `True`/`False` and normalize → `IfBlock` (match elm-syntax)

Option 2 is preferable since it preserves source intent.
