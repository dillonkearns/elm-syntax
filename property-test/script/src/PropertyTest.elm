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


compareSource : String -> String -> BackendTask FatalError TestResult
compareSource moduleName source =
    case Elm.Parser.parseToFile source of
        Err _ ->
            -- elm-syntax couldn't parse — check if elm-format can
            BackendTask.Custom.run "parseWithElmFormat"
                (Encode.string source)
                Decode.value
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (\elmFormatJson ->
                        let
                            isNull =
                                Decode.decodeValue (Decode.null True) elmFormatJson
                                    |> Result.withDefault False
                        in
                        if isNull then
                            Script.log ("  [skip] " ++ moduleName ++ " — both parsers failed")
                                |> BackendTask.map (\_ -> Error "both parsers failed")

                        else
                            Script.log ("  [DISAGREE] " ++ moduleName ++ " — elm-syntax REJECTED but elm-format ACCEPTED")
                                |> BackendTask.map (\_ -> Fail "elm-syntax rejected valid source")
                    )

        Ok file ->
            let
                syntaxCanonical : CanonicalFile
                syntaxCanonical =
                    Canonical.FromElmSyntax.fromFile file
            in
            BackendTask.Custom.run "parseWithElmFormat"
                (Encode.string source)
                Decode.value
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (\elmFormatJson ->
                        let
                            isNull =
                                Decode.decodeValue (Decode.null True) elmFormatJson
                                    |> Result.withDefault False
                        in
                        if isNull then
                            Script.log ("  [DISAGREE] " ++ moduleName ++ " — elm-syntax ACCEPTED but elm-format REJECTED")
                                |> BackendTask.map (\_ -> Fail "elm-syntax accepted invalid source")

                        else
                            case Decode.decodeValue Canonical.FromElmFormatJson.decoder elmFormatJson of
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
                    )


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
