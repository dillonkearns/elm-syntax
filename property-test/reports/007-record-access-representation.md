# Difference: Record access representation

**Severity**: Low (semantically equivalent)
**Found by**: seed 999, Test0

## Source

```elm
b.result
```

## Difference

| Parser | Representation |
|--------|---------------|
| elm-syntax | `RecordAccess (FunctionOrValue "b") "result"` |
| elm-format (Elm compiler) | `Application [RecordAccessFunction ".result", FunctionOrValue "b"]` |

elm-syntax represents `b.result` as a dedicated `RecordAccess` node, while elm-format/the Elm compiler desugars it to function application of the record access function `.result` to `b`.

## Impact

Semantically equivalent — `b.result` and `.result b` evaluate to the same thing.

## Normalization approach

Normalize `RecordAccess expr field` → `Application [RecordAccessFunction field, expr]` (match elm-format).
Or normalize in the other direction.
