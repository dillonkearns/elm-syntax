module EdgeCaseTests exposing (run)

{-| Targeted edge case tests based on whitebox analysis of the parser.
Tests specific tricky constructs rather than random generation.
-}

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import Canonical.Diff as Diff
import Canonical.FromElmFormatJson
import Canonical.FromElmSyntax
import Elm.Parser
import Elm.Syntax.File
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions
        (runAll edgeCases 0 { passed = 0, failed = 0, errors = 0 }
            |> BackendTask.andThen
                (\stats ->
                    Script.log
                        ("\n=== Edge Case Results ===\n"
                            ++ "Passed: "
                            ++ String.fromInt stats.passed
                            ++ "\nFailed: "
                            ++ String.fromInt stats.failed
                            ++ "\nErrors: "
                            ++ String.fromInt stats.errors
                            ++ "\nTotal:  "
                            ++ String.fromInt (stats.passed + stats.failed + stats.errors)
                        )
                )
        )


type alias Stats =
    { passed : Int, failed : Int, errors : Int }


type alias TestCase =
    { name : String
    , source : String
    }


runAll : List TestCase -> Int -> Stats -> BackendTask FatalError Stats
runAll cases index stats =
    case cases of
        [] ->
            BackendTask.succeed stats

        tc :: rest ->
            runOne tc
                |> BackendTask.andThen
                    (\result ->
                        let
                            newStats =
                                case result of
                                    Pass ->
                                        { stats | passed = stats.passed + 1 }

                                    Fail _ ->
                                        { stats | failed = stats.failed + 1 }

                                    Error _ ->
                                        { stats | errors = stats.errors + 1 }
                        in
                        runAll rest (index + 1) newStats
                    )


type TestResult
    = Pass
    | Fail String
    | Error String


runOne : TestCase -> BackendTask FatalError TestResult
runOne tc =
    let
        elmSyntaxResult =
            Elm.Parser.parseToFile tc.source
    in
    BackendTask.Custom.run "parseWithElmFormat"
        (Encode.string tc.source)
        (Decode.map2 (\s a -> ( s, a ))
            (Decode.field "status" Decode.string)
            (Decode.maybe (Decode.field "ast" Decode.value))
        )
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\( fmtStatus, fmtAst ) ->
                case ( elmSyntaxResult, fmtStatus ) of
                    ( _, "crash" ) ->
                        Script.log ("  [skip] " ++ tc.name ++ " — elm-format crash")
                            |> BackendTask.map (\_ -> Error "crash")

                    ( Err _, "parse-error" ) ->
                        Script.log ("  [skip] " ++ tc.name ++ " — both reject")
                            |> BackendTask.map (\_ -> Error "both reject")

                    ( Err _, "error" ) ->
                        Script.log ("  [skip] " ++ tc.name ++ " — both reject")
                            |> BackendTask.map (\_ -> Error "both reject")

                    ( Ok _, "parse-error" ) ->
                        -- elm-syntax accepts, elm-format rejects — check with elm make
                        Script.log ("  [check] " ++ tc.name ++ " — elm-syntax accepts, elm-format rejects (need elm make)")
                            |> BackendTask.map (\_ -> Error "needs elm make check")

                    ( Err _, "ok" ) ->
                        -- elm-syntax rejects, elm-format accepts — potential bug!
                        -- But elm-format can be overly permissive, need elm make to confirm
                        Script.log
                            ("\n  [POTENTIAL BUG] "
                                ++ tc.name
                                ++ " — elm-syntax REJECTS but elm-format accepts\n  Source: "
                                ++ tc.source
                            )
                            |> BackendTask.map (\_ -> Fail "elm-syntax rejects valid?")

                    ( Ok file, "ok" ) ->
                        case fmtAst of
                            Just ast ->
                                case Decode.decodeValue Canonical.FromElmFormatJson.decoder ast of
                                    Ok fmtCanonical ->
                                        let
                                            syntaxCanonical =
                                                Canonical.FromElmSyntax.fromFile file

                                            diffs =
                                                Diff.diff syntaxCanonical fmtCanonical
                                        in
                                        if List.isEmpty diffs then
                                            Script.log ("  [pass] " ++ tc.name)
                                                |> BackendTask.map (\_ -> Pass)

                                        else
                                            Script.log
                                                ("\n  [DIFF] "
                                                    ++ tc.name
                                                    ++ "\n  Source: "
                                                    ++ tc.source
                                                    ++ "\n"
                                                    ++ Diff.formatDiff diffs
                                                )
                                                |> BackendTask.map (\_ -> Fail "AST diff")

                                    Err e ->
                                        Script.log ("  [error] " ++ tc.name ++ " — decode: " ++ Decode.errorToString e)
                                            |> BackendTask.map (\_ -> Error "decode")

                            Nothing ->
                                Script.log ("  [error] " ++ tc.name ++ " — no ast")
                                    |> BackendTask.map (\_ -> Error "no ast")

                    _ ->
                        Script.log ("  [skip] " ++ tc.name)
                            |> BackendTask.map (\_ -> Error "unknown")
            )


edgeCases : List TestCase
edgeCases =
    -- === NEGATION EDGE CASES ===
    [ t "neg-in-list" "module T exposing (..)\nx = [-3]"
    , t "neg-in-list-multi" "module T exposing (..)\nx = [-3, -4, -5]"
    , t "neg-in-tuple" "module T exposing (..)\nx = (-3, -4)"
    , t "neg-after-comma" "module T exposing (..)\nx = [1, -2]"
    , t "neg-after-open-paren" "module T exposing (..)\nx = (-3)"
    , t "neg-nested-list" "module T exposing (..)\nx = [[-3]]"
    , t "neg-in-record" "module T exposing (..)\nx = { a = -3 }"
    , t "neg-in-if-cond" "module T exposing (..)\nx = if -1 then 1 else 2"
    , t "neg-in-case-subject" "module T exposing (..)\nx = case -1 of\n        _ -> 1"
    , t "neg-float-in-list" "module T exposing (..)\nx = [-3.5]"
    , t "neg-hex-in-list" "module T exposing (..)\nx = [-0xFF]"
    , t "neg-start-of-expr" "module T exposing (..)\nx = -3"
    , t "neg-in-func-arg" "module T exposing (..)\nx = f -3"
    , t "neg-double" "module T exposing (..)\nx = - -3"
    , t "neg-of-parens" "module T exposing (..)\nx = -(3)"

    -- === OPERATOR EDGE CASES ===
    , t "op-start-of-line" "module T exposing (..)\nx =\n    1\n    + 2"
    , t "op-pipeline" "module T exposing (..)\nx = a |> b |> c"
    , t "op-left-pizza" "module T exposing (..)\nx = c <| b <| a"
    , t "op-compose" "module T exposing (..)\nx = a >> b >> c"
    , t "op-mixed-precedence" "module T exposing (..)\nx = 1 + 2 * 3"
    , t "op-right-assoc" "module T exposing (..)\nx = a :: b :: []"
    , t "op-append" "module T exposing (..)\nx = a ++ b ++ c"
    , t "op-compare-chain" "module T exposing (..)\nx = a == b"

    -- === RECORD EDGE CASES ===
    , t "record-empty" "module T exposing (..)\nx = {}"
    , t "record-update" "module T exposing (..)\nx = { a | b = 1 }"
    , t "record-update-multi" "module T exposing (..)\nx = { a | b = 1, c = 2 }"
    , t "record-nested-update" "module T exposing (..)\nx = { a | b = { c | d = 1 } }"
    , t "record-access-chain" "module T exposing (..)\nx = a.b.c"
    , t "record-access-func-apply" "module T exposing (..)\nx = .field record"
    , t "record-in-pattern" "module T exposing (..)\nf { a, b } = a"
    , t "record-type-alias" "module T exposing (..)\ntype alias R = { a : Int, b : String }"

    -- === PATTERN EDGE CASES ===
    , t "pattern-nested-cons" "module T exposing (..)\nf x = case x of\n        a :: b :: c -> 1\n        _ -> 2"
    , t "pattern-tuple-in-cons" "module T exposing (..)\nf x = case x of\n        (a, b) :: rest -> 1\n        _ -> 2"
    , t "pattern-as-nested" "module T exposing (..)\nf x = case x of\n        (Just y) as z -> z\n        _ -> x"
    , t "pattern-named-args" "module T exposing (..)\nf x = case x of\n        Just (Just y) -> y\n        _ -> Nothing"
    , t "pattern-wildcard-as" "module T exposing (..)\nf x = case x of\n        _ as y -> y"
    , t "pattern-unit" "module T exposing (..)\nf x = case x of\n        () -> 1"
    , t "pattern-empty-list" "module T exposing (..)\nf x = case x of\n        [] -> 1\n        _ -> 2"
    , t "pattern-literal-list" "module T exposing (..)\nf x = case x of\n        [a] -> a\n        [a, b] -> a\n        _ -> 0"

    -- === STRING/CHAR EDGE CASES ===
    , t "string-escape-newline" "module T exposing (..)\nx = \"hello\\nworld\""
    , t "string-escape-tab" "module T exposing (..)\nx = \"hello\\tworld\""
    , t "string-escape-quote" "module T exposing (..)\nx = \"she said \\\"hi\\\"\""
    , t "string-escape-backslash" "module T exposing (..)\nx = \"path\\\\to\\\\file\""
    , t "string-unicode" "module T exposing (..)\nx = \"\\u{0041}\""
    , t "string-multiline" "module T exposing (..)\nx = \"\"\"hello\nworld\"\"\""
    , t "string-multiline-quotes" "module T exposing (..)\nx = \"\"\"she said \"hi\"\"\"\""
    , t "char-escape-single-quote" "module T exposing (..)\nx = '\\''"
    , t "char-escape-backslash" "module T exposing (..)\nx = '\\\\'"
    , t "char-unicode" "module T exposing (..)\nx = '\\u{0041}'"

    -- === LET EDGE CASES ===
    , t "let-destructure-tuple" "module T exposing (..)\nx =\n    let\n        (a, b) = (1, 2)\n    in\n    a"
    , t "let-destructure-record" "module T exposing (..)\nx =\n    let\n        { a, b } = r\n    in\n    a"
    , t "let-multiple-bindings" "module T exposing (..)\nx =\n    let\n        a = 1\n        b = 2\n        c = 3\n    in\n    a + b + c"
    , t "let-func-with-args" "module T exposing (..)\nx =\n    let\n        f a b = a + b\n    in\n    f 1 2"
    , t "let-with-type-ann" "module T exposing (..)\nx =\n    let\n        f : Int -> Int\n        f a = a + 1\n    in\n    f 1"

    -- === TYPE ANNOTATION EDGE CASES ===
    , t "type-extensible-record" "module T exposing (..)\ntype alias F a = { a | name : String }"
    , t "type-nested-parens" "module T exposing (..)\ntype alias F = ((Int -> String) -> Bool)"
    , t "type-deeply-nested" "module T exposing (..)\ntype alias F = Maybe (List (Result String (Maybe Int)))"
    , t "type-func-in-tuple" "module T exposing (..)\ntype alias F = ( Int -> String, Bool )"

    -- === LAMBDA EDGE CASES ===
    , t "lambda-multi-arg" "module T exposing (..)\nx = \\a b c -> a + b + c"
    , t "lambda-pattern-arg" "module T exposing (..)\nx = \\(a, b) -> a + b"
    , t "lambda-nested" "module T exposing (..)\nx = \\a -> \\b -> a + b"

    -- === MISC TRICKY CONSTRUCTS ===
    , t "if-multiline" "module T exposing (..)\nx =\n    if True\n    then 1\n    else 2"
    , t "case-multiline" "module T exposing (..)\nx =\n    case y of\n        Just a ->\n            a\n\n        Nothing ->\n            0"
    , t "tuple-singleton-parens" "module T exposing (..)\nx = (1)"
    , t "empty-list" "module T exposing (..)\nx = []"
    , t "port-command" "port module T exposing (..)\nport send : String -> Cmd msg"
    , t "port-subscription" "port module T exposing (..)\nport receive : (String -> msg) -> Sub msg"
    , t "infix-as-value" "module T exposing (..)\nx = (+) 1 2"
    , t "section-left" "module T exposing (..)\nx = (+ 1)"
    , t "section-right" "module T exposing (..)\nx = (1 +)"
    ]


t : String -> String -> TestCase
t name source =
    { name = name, source = source }
