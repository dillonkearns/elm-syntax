module PropertyTest exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import Canonical exposing (CanonicalFile)
import Canonical.Diff as Diff
import Canonical.FromElmFormatJson
import Canonical.FromElmSyntax
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm.Parser
import Elm.Syntax.File
import FatalError exposing (FatalError)
import Generate.Module as Module
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Random


run : Script
run =
    Script.withCliOptions program
        (\options ->
            let
                seed : Random.Seed
                seed =
                    Random.initialSeed
                        (case options.seed of
                            Just s ->
                                s

                            Nothing ->
                                42
                        )
            in
            runTests seed options.count 0 { passed = 0, failed = 0, errors = 0 }
                |> BackendTask.andThen
                    (\stats ->
                        Script.log
                            ("\n=== Results ===\n"
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
    { passed : Int
    , failed : Int
    , errors : Int
    }


runTests : Random.Seed -> Int -> Int -> Stats -> BackendTask FatalError Stats
runTests seed count index stats =
    if index >= count then
        BackendTask.succeed stats

    else
        let
            moduleName : String
            moduleName =
                "Test" ++ String.fromInt index

            ( source, nextSeed ) =
                Random.step (Module.generator moduleName 2) seed
        in
        compareSource moduleName source
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
                    runTests nextSeed count (index + 1) newStats
                )


type TestResult
    = Pass
    | Fail String
    | Error String


type alias ElmFormatResult =
    { status : String
    , ast : Maybe Decode.Value
    }


elmFormatResultDecoder : Decode.Decoder ElmFormatResult
elmFormatResultDecoder =
    Decode.map2 ElmFormatResult
        (Decode.field "status" Decode.string)
        (Decode.maybe (Decode.field "ast" Decode.value))


compareSource : String -> String -> BackendTask FatalError TestResult
compareSource moduleName source =
    BackendTask.Custom.run "parseWithElmFormat"
        (Encode.string source)
        elmFormatResultDecoder
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\elmFormatResult ->
                let
                    elmSyntaxResult =
                        Elm.Parser.parseToFile source
                in
                case ( elmSyntaxResult, elmFormatResult.status ) of
                    ( _, "crash" ) ->
                        -- elm-format crashed internally — skip this test
                        Script.log ("  [skip] " ++ moduleName ++ " — elm-format crashed")
                            |> BackendTask.map (\_ -> Error "elm-format crash")

                    ( Err _, "parse-error" ) ->
                        -- Both failed to parse — generated source was invalid
                        Script.log ("  [skip] " ++ moduleName ++ " — both parsers failed")
                            |> BackendTask.map (\_ -> Error "both parsers failed")

                    ( Err _, "error" ) ->
                        Script.log ("  [skip] " ++ moduleName ++ " — both parsers failed")
                            |> BackendTask.map (\_ -> Error "both parsers failed")

                    ( Err _, "ok" ) ->
                        -- elm-syntax rejected but elm-format accepted — potential elm-syntax bug
                        Script.log
                            ("\n  [DISAGREE] "
                                ++ moduleName
                                ++ " — elm-syntax REJECTED but elm-format ACCEPTED\nSource:\n"
                                ++ source
                            )
                            |> BackendTask.map (\_ -> Fail "elm-syntax rejected valid source")

                    ( Ok _, "parse-error" ) ->
                        -- elm-syntax accepted but elm-format rejected — potential false acceptance
                        Script.log
                            ("\n  [DISAGREE] "
                                ++ moduleName
                                ++ " — elm-syntax ACCEPTED but elm-format REJECTED\nSource:\n"
                                ++ source
                            )
                            |> BackendTask.map (\_ -> Fail "elm-syntax accepted invalid source")

                    ( Ok file, "ok" ) ->
                        -- Both parsed — compare ASTs
                        case elmFormatResult.ast of
                            Just formatJson ->
                                compareAsts moduleName source file formatJson

                            Nothing ->
                                Script.log ("  [error] " ++ moduleName ++ " — elm-format OK but no AST")
                                    |> BackendTask.map (\_ -> Error "missing ast")

                    ( _, _ ) ->
                        Script.log ("  [skip] " ++ moduleName ++ " — elm-format status: " ++ elmFormatResult.status)
                            |> BackendTask.map (\_ -> Error "unknown status")
            )


compareAsts : String -> String -> Elm.Syntax.File.File -> Decode.Value -> BackendTask FatalError TestResult
compareAsts moduleName source file formatJson =
    let
        syntaxCanonical =
            Canonical.FromElmSyntax.fromFile file
    in
    case Decode.decodeValue Canonical.FromElmFormatJson.decoder formatJson of
        Ok formatCanonical ->
            let
                diffs =
                    Diff.diff syntaxCanonical formatCanonical
            in
            if List.isEmpty diffs then
                Script.log ("  [pass] " ++ moduleName)
                    |> BackendTask.map (\_ -> Pass)

            else
                Script.log
                    ("\n  [FAIL] "
                        ++ moduleName
                        ++ "\nSource:\n"
                        ++ source
                        ++ "\nDiffs:\n"
                        ++ Diff.formatDiff diffs
                    )
                    |> BackendTask.map (\_ -> Fail (Diff.formatDiff diffs))

        Err formatErr ->
            Script.log
                ("  [error] "
                    ++ moduleName
                    ++ " — elm-format JSON decode error: "
                    ++ Decode.errorToString formatErr
                )
                |> BackendTask.map (\_ -> Error "elm-format decode error")


type alias CliOptions =
    { seed : Maybe Int
    , count : Int
    }


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.optionalKeywordArg "seed"
                        |> Option.validateMap
                            (\maybeSeed ->
                                case maybeSeed of
                                    Nothing ->
                                        Ok Nothing

                                    Just seedStr ->
                                        case String.toInt seedStr of
                                            Just n ->
                                                Ok (Just n)

                                            Nothing ->
                                                Err "seed must be an integer"
                            )
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "count"
                        |> Option.withDefault "20"
                        |> Option.validateMap
                            (\countStr ->
                                case String.toInt countStr of
                                    Just n ->
                                        Ok n

                                    Nothing ->
                                        Err "count must be an integer"
                            )
                    )
            )
