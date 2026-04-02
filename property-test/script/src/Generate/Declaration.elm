module Generate.Declaration exposing (generator)

{-| Generate random Elm declaration source strings.
-}

import Generate.Expression as Expression exposing (topLevelGenerator)
import Generate.Identifier as Identifier
import Generate.TypeAnnotation as TypeAnnotation
import Random exposing (Generator)


{-| Generate a random declaration source string.
-}
generator : Int -> Generator String
generator depth =
    Random.uniform (functionDecl depth)
        [ typeAliasDecl depth
        , customTypeDecl depth
        , portDecl depth
        ]
        |> Random.andThen identity


functionDecl : Int -> Generator String
functionDecl depth =
    Random.map2
        (\( name, args ) ( maybeSig, body ) ->
            let
                sigLine =
                    case maybeSig of
                        Just sig ->
                            name ++ " : " ++ sig ++ "\n"

                        Nothing ->
                            ""

                argsStr =
                    if List.isEmpty args then
                        ""

                    else
                        " " ++ String.join " " args
            in
            sigLine ++ name ++ argsStr ++ " =\n    " ++ body
        )
        (Random.map2 Tuple.pair
            Identifier.lowerName
            (Random.int 0 3
                |> Random.andThen (\n -> randomList n Identifier.lowerName)
            )
        )
        (Random.map2 Tuple.pair
            (Random.uniform True [ False ]
                |> Random.andThen
                    (\hasSig ->
                        if hasSig then
                            TypeAnnotation.generator 1 |> Random.map Just

                        else
                            Random.constant Nothing
                    )
            )
            (topLevelGenerator depth)
        )


typeAliasDecl : Int -> Generator String
typeAliasDecl depth =
    Random.map3
        (\name generics ta ->
            let
                genericsStr =
                    if List.isEmpty generics then
                        ""

                    else
                        " " ++ String.join " " generics
            in
            "type alias " ++ name ++ genericsStr ++ " =\n    " ++ ta
        )
        Identifier.upperName
        (Random.int 0 2
            |> Random.andThen (\n -> randomList n Identifier.lowerName)
        )
        (TypeAnnotation.generator depth)


customTypeDecl : Int -> Generator String
customTypeDecl depth =
    Random.map3
        (\name generics variants ->
            let
                genericsStr =
                    if List.isEmpty generics then
                        ""

                    else
                        " " ++ String.join " " generics

                variantsStr =
                    variants
                        |> List.indexedMap
                            (\i v ->
                                if i == 0 then
                                    "= " ++ v

                                else
                                    "| " ++ v
                            )
                        |> String.join "\n    "
            in
            "type " ++ name ++ genericsStr ++ "\n    " ++ variantsStr
        )
        Identifier.upperName
        (Random.int 0 2
            |> Random.andThen (\n -> randomList n Identifier.lowerName)
        )
        (Random.int 1 4
            |> Random.andThen (\n -> randomList n (variantDecl depth))
        )


variantDecl : Int -> Generator String
variantDecl depth =
    Random.map2
        (\name args ->
            if List.isEmpty args then
                name

            else
                name ++ " " ++ String.join " " args
        )
        Identifier.upperName
        (Random.int 0 3
            |> Random.andThen
                (\n ->
                    randomList n
                        (TypeAnnotation.generator (depth - 1)
                            |> Random.map
                                (\t ->
                                    if String.contains " " t && not (String.startsWith "(" t) && not (String.startsWith "{" t) then
                                        "(" ++ t ++ ")"

                                    else
                                        t
                                )
                        )
                )
        )



portDecl : Int -> Generator String
portDecl depth =
    Random.map2
        (\name typeAnn ->
            "port " ++ name ++ " : " ++ typeAnn
        )
        Identifier.lowerName
        (TypeAnnotation.generator depth)



-- HELPERS


randomList : Int -> Generator a -> Generator (List a)
randomList n gen =
    if n <= 0 then
        Random.constant []

    else
        Random.map2 (::) gen (randomList (n - 1) gen)
