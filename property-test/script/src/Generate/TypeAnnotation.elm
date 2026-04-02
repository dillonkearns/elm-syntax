module Generate.TypeAnnotation exposing (generator)

{-| Generate random Elm type annotation source strings.
-}

import Generate.Identifier as Identifier
import Random exposing (Generator)


{-| Generate a random type annotation source string at a given depth.
-}
generator : Int -> Generator String
generator depth =
    if depth <= 0 then
        leaf

    else
        Random.uniform leaf
            [ maybeType depth
            , listType depth
            , resultType depth
            , tupleType depth
            , recordType depth
            , functionType depth
            ]
            |> Random.andThen identity


leaf : Generator String
leaf =
    Random.uniform "Int" [ "String", "Bool", "Float", "()" ]


maybeType : Int -> Generator String
maybeType depth =
    generator (depth - 1) |> Random.map (\t -> "Maybe " ++ wrapIfComplex t)


listType : Int -> Generator String
listType depth =
    generator (depth - 1) |> Random.map (\t -> "List " ++ wrapIfComplex t)


resultType : Int -> Generator String
resultType depth =
    Random.map2
        (\err ok -> "Result " ++ wrapIfComplex err ++ " " ++ wrapIfComplex ok)
        (generator (depth - 1))
        (generator (depth - 1))


tupleType : Int -> Generator String
tupleType depth =
    Random.int 2 3
        |> Random.andThen (\n -> randomList n (generator (depth - 1)))
        |> Random.map (\items -> "( " ++ String.join ", " items ++ " )")


recordType : Int -> Generator String
recordType depth =
    Random.int 1 4
        |> Random.andThen
            (\n ->
                randomList n
                    (Random.map2
                        (\name ta -> name ++ " : " ++ ta)
                        Identifier.lowerName
                        (generator (depth - 1))
                    )
            )
        |> Random.map (\fields -> "{ " ++ String.join ", " fields ++ " }")


functionType : Int -> Generator String
functionType depth =
    Random.map2
        (\from to -> from ++ " -> " ++ to)
        (generator (depth - 1))
        (generator (depth - 1))



-- HELPERS


wrapIfComplex : String -> String
wrapIfComplex t =
    if String.contains " " t && not (String.startsWith "(" t) && not (String.startsWith "{" t) then
        "(" ++ t ++ ")"

    else
        t


randomList : Int -> Generator a -> Generator (List a)
randomList n gen =
    if n <= 0 then
        Random.constant []

    else
        Random.map2 (::) gen (randomList (n - 1) gen)
