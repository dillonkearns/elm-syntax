# Difference: Duplicate record field handling

**Severity**: Low (only affects invalid Elm — duplicate record fields are rejected by the type checker)
**Found by**: seed 200, Tests 4/5/8/14/16/42

## Source examples

```elm
type alias Alpha alpha bar =
    Maybe { msg : (), a : (), msg : Bool, b : Bool }
```

```elm
type alias Variant =
    { model : String, b : Float, model : () }
```

```elm
{ model = { ... }, model = if "world" then .a else g, tail = ... }
```

## Difference

| Parser | Behavior with duplicate fields |
|--------|-------------------------------|
| elm-syntax | Preserves all fields in source order (first occurrence keeps its type) |
| elm-format (Elm compiler) | Keeps all field entries but uses the **last** definition's type for duplicates |

For `{ msg : (), msg : Bool }`:
- elm-syntax: `[("msg", ()), ("msg", Bool)]` — first `msg` has type `()`
- elm-format: `[("msg", Bool), ("msg", Bool)]` — both `msg` entries have type `Bool` (last wins)

For record expressions with duplicate fields:
- elm-syntax: keeps all fields
- elm-format: deduplicates, keeping only the last occurrence

## Impact

Duplicate record fields are invalid Elm — the compiler rejects them at type-checking. This difference only manifests in code that would never compile successfully. No impact on valid programs.

## Normalization approach

For record types and expressions with duplicate field names, keep only the last occurrence of each field name (matching elm-format behavior). Or simply skip comparison when duplicates are detected.
