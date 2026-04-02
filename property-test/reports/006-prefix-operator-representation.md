# Difference: Prefix operator representation

**Severity**: Low (semantically equivalent)  
**Found by**: seed 42, Test56

## Source

```elm
x =
    if { model = 424.39, result = ' ', value = (&&), a = () } then ( xs, (-), (*) ) else -'z'
```

## Difference

| Parser | Representation of `(&&)` |
|--------|------------------------|
| elm-syntax | `PrefixOperator "&&"` |
| elm-format (Elm compiler) | `FunctionOrValue [] "&&"` |

elm-syntax has a dedicated `PrefixOperator` AST node for operators in prefix position like `(+)`, while elm-format treats them as regular variable references.

## Impact

Semantically equivalent — `(&&)` is just the function reference for the `&&` operator. Both representations refer to the same thing.

## Normalization approach

Normalize `PrefixOperator op` → `FunctionOrValue [] op`.
