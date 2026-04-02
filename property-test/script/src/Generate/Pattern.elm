module Generate.Pattern exposing (generator)

{-| Generate random Elm pattern source strings.
-}

import Generate.Identifier as Identifier
import Random exposing (Generator)


{-| Generate a random pattern source string at a given depth.
-}
generator : Int -> Generator String
generator depth =
    if depth <= 0 then
        leaf

    else
        Random.uniform leaf
            [ tuplePattern depth
            , consPattern depth
            , listPattern depth
            , namedPattern depth
            , asPattern depth
            ]
            |> Random.andThen identity


leaf : Generator String
leaf =
    Random.uniform wildcard
        [ unitPattern
        , varPattern
        , intPattern
        , stringPattern
        , charPattern
        ]
        |> Random.andThen identity


wildcard : Generator String
wildcard =
    Random.constant "_"


unitPattern : Generator String
unitPattern =
    Random.constant "()"


varPattern : Generator String
varPattern =
    Identifier.lowerName


intPattern : Generator String
intPattern =
    Random.int -99 99 |> Random.map String.fromInt


stringPattern : Generator String
stringPattern =
    Random.uniform "hello" [ "world", "foo", "", "test" ]
        |> Random.map (\s -> "\"" ++ s ++ "\"")


charPattern : Generator String
charPattern =
    Random.uniform 'a' [ 'b', 'z', '0' ]
        |> Random.map (\c -> "'" ++ String.fromChar c ++ "'")


tuplePattern : Int -> Generator String
tuplePattern depth =
    Random.int 2 3
        |> Random.andThen (\n -> randomList n (generator (depth - 1)))
        |> Random.map (\items -> "( " ++ String.join ", " items ++ " )")


consPattern : Int -> Generator String
consPattern depth =
    Random.map2
        (\head tail -> head ++ " :: " ++ tail)
        (generator (depth - 1))
        (generator (depth - 1))


listPattern : Int -> Generator String
listPattern depth =
    Random.int 0 3
        |> Random.andThen (\n -> randomList n (generator (depth - 1)))
        |> Random.map (\items -> "[ " ++ String.join ", " items ++ " ]")


namedPattern : Int -> Generator String
namedPattern depth =
    Random.map2
        (\name args ->
            if List.isEmpty args then
                name

            else
                name ++ " " ++ String.join " " (List.map (\a -> "(" ++ a ++ ")") args)
        )
        Identifier.upperName
        (Random.int 0 2
            |> Random.andThen (\n -> randomList n (generator (depth - 1)))
        )


asPattern : Int -> Generator String
asPattern depth =
    Random.map2
        (\pat name -> "(" ++ pat ++ ") as " ++ name)
        (generator (depth - 1))
        Identifier.lowerName



-- HELPERS


randomList : Int -> Generator a -> Generator (List a)
randomList n gen =
    if n <= 0 then
        Random.constant []

    else
        Random.map2 (::) gen (randomList (n - 1) gen)
