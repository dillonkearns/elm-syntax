module DiagnoseSkips exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import FatalError exposing (FatalError)
import Generate.Module as Module
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Random


run : Script
run =
    Script.withoutCliOptions
        (let
            seed =
                Random.initialSeed 42
         in
         diagnose seed 10 0
        )


diagnose : Random.Seed -> Int -> Int -> BackendTask FatalError ()
diagnose seed count index =
    if index >= count then
        BackendTask.succeed ()

    else
        let
            moduleName =
                "Test" ++ String.fromInt index

            ( source, nextSeed ) =
                Random.step (Module.generator moduleName 2) seed
        in
        BackendTask.Custom.run "parseWithElmSyntax"
            (Encode.string source)
            (Decode.field "parsed" Decode.bool)
            |> BackendTask.allowFatal
            |> BackendTask.andThen
                (\parsed ->
                    if not parsed then
                        Script.log
                            ("\n--- " ++ moduleName ++ " FAILED TO PARSE ---\n" ++ source ++ "\n--- END ---")
                            |> BackendTask.andThen (\_ -> diagnose nextSeed count (index + 1))

                    else
                        diagnose nextSeed count (index + 1)
                )
