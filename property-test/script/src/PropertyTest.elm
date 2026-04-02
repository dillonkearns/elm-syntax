module PropertyTest exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import Canonical exposing (CanonicalFile)
import Canonical.Diff as Diff
import Canonical.Encode as Encode
import Canonical.FromElmFormatJson
import Canonical.FromElmSyntaxJson
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withCliOptions program
        (\options ->
            let
                testSource : String
                testSource =
                    "module Test1 exposing (..)\n\nadd : Int -> Int -> Int\nadd a b = a + b\n\ntype Msg = Click | Hover String\n\ntype alias Model = { count : Int, name : String }\n\ngreet name =\n    case name of\n        \"world\" -> \"Hello, world!\"\n        _ -> \"Hi, \" ++ name\n"
            in
            compareSource testSource
        )


compareSource : String -> BackendTask FatalError ()
compareSource source =
    BackendTask.Custom.run "parseWithElmSyntax"
        (Encode.string source)
        Decode.value
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\elmSyntaxJson ->
                BackendTask.Custom.run "parseWithElmFormat"
                    (Encode.string source)
                    Decode.value
                    |> BackendTask.allowFatal
                    |> BackendTask.andThen
                        (\elmFormatJson ->
                            let
                                elmSyntaxResult =
                                    Decode.decodeValue Canonical.FromElmSyntaxJson.decoder elmSyntaxJson

                                elmFormatResult =
                                    Decode.decodeValue Canonical.FromElmFormatJson.decoder elmFormatJson
                            in
                            case ( elmSyntaxResult, elmFormatResult ) of
                                ( Ok syntaxCanonical, Ok formatCanonical ) ->
                                    let
                                        diffs =
                                            Diff.diff syntaxCanonical formatCanonical
                                    in
                                    if List.isEmpty diffs then
                                        Script.log "PASS: Both parsers agree on the AST"

                                    else
                                        Script.log
                                            ("MISMATCH:\n"
                                                ++ "Source:\n"
                                                ++ source
                                                ++ "\n"
                                                ++ Diff.formatDiff diffs
                                            )

                                ( Err syntaxErr, _ ) ->
                                    Script.log
                                        ("ERROR decoding elm-syntax JSON:\n"
                                            ++ Decode.errorToString syntaxErr
                                            ++ "\n\nRaw JSON:\n"
                                            ++ Encode.encode 2 elmSyntaxJson
                                        )

                                ( _, Err formatErr ) ->
                                    Script.log
                                        ("ERROR decoding elm-format JSON:\n"
                                            ++ Decode.errorToString formatErr
                                            ++ "\n\nRaw JSON:\n"
                                            ++ Encode.encode 2 elmFormatJson
                                        )
                        )
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
