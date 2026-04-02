# BUG: Negative numbers rejected after keywords without space

**Severity**: Medium — affects valid Elm code
**Status**: Confirmed bugs in elm-syntax parser
**Found by**: systematic negation context testing
**Related to**: #008 (same negation detection mechanism)

## Reproduction

```elm
-- All of these compile with `elm make` but are rejected by elm-syntax:

x = if True then-1 else 0

y = if True then 0 else-1

z = let a = 1 in-1
```

## Root cause

The negation detector in `Expression.elm` checks a single character before `-`.
When a keyword (`then`, `else`, `in`) appears immediately before `-` with no
space, the preceding character is a letter (`n` or `e`), which falls through to
the `_ ->` catch-all that treats `-` as subtraction.

Unlike the `=` fix (#008), this can't be solved by adding single characters to
the match set — adding `n` or `e` would break `x-1` (treating subtraction as
negation after any variable ending in `n` or `e`).

## Suggested fix

The parser needs to check whether the preceding **token** was a keyword, not
just the single preceding character. Options:

1. Check a longer slice (e.g., 4-5 chars back) against known keyword endings
2. Restructure the parser to track whether the previous token was a keyword
3. Check if the preceding characters form a keyword by slicing back to the
   previous word boundary

The keywords that can precede an expression are: `then`, `else`, `in`, `of`
(though `of` already works because case branches have mandatory newlines).
