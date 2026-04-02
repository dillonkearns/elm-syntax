port module ParseWorker exposing (main)

import Elm.Parser as Parser
import Elm.Syntax.File exposing (File)
import Json.Encode as Encode


port requestParsing : (String -> msg) -> Sub msg


port parseResult : Encode.Value -> Cmd msg


main : Program () () Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \msg _ -> ( (), update msg )
        , subscriptions = always subscriptions
        }


subscriptions : Sub Msg
subscriptions =
    requestParsing GotFile


type Msg
    = GotFile String


update : Msg -> Cmd Msg
update (GotFile source) =
    let
        json : Encode.Value
        json =
            case Parser.parseToFile source of
                Ok ast ->
                    Encode.object
                        [ ( "parsed", Encode.bool True )
                        , ( "ast", Elm.Syntax.File.encode ast )
                        ]

                Err deadEnds ->
                    Encode.object
                        [ ( "parsed", Encode.bool False )
                        , ( "error"
                          , deadEnds
                                |> List.map
                                    (\de ->
                                        Encode.object
                                            [ ( "row", Encode.int de.row )
                                            , ( "col", Encode.int de.col )
                                            ]
                                    )
                                |> Encode.list identity
                          )
                        ]
    in
    parseResult json
