module Generate.Expression exposing (generator, topLevelGenerator)

{-| Generate random Elm expression source strings.
Depth parameter controls recursion to prevent infinite generation.
-}

import Generate.Identifier as Identifier
import Generate.Pattern as Pattern
import Hex
import Random exposing (Generator)


{-| Generate a random expression source string at a given depth.
-}
generator : Int -> Generator String
generator depth =
    if depth <= 0 then
        leaf

    else
        -- Separate "inline-safe" expressions (work anywhere) from
        -- "block" expressions (need to be at the top of a function body
        -- due to indentation requirements).
        Random.uniform leaf
            [ application depth
            , operatorExpr depth
            , ifExpr depth
            , lambdaExpr depth
            , tupleExpr depth
            , listExpr depth
            , recordExpr depth
            , negation depth
            , parenExpr depth
            , recordAccessExpr depth
            , recordUpdateExpr depth
            ]
            |> Random.andThen identity


{-| Generate expressions suitable for a function body (top-level context).
Includes let and case expressions that require specific indentation.
-}
topLevelGenerator : Int -> Generator String
topLevelGenerator depth =
    if depth <= 0 then
        leaf

    else
        Random.uniform (generator depth)
            [ caseExpr depth
            , letExpr depth
            ]
            |> Random.andThen identity


leaf : Generator String
leaf =
    Random.uniform intLiteral
        [ floatLiteral
        , stringLiteral
        , charLiteral
        , unitLiteral
        , variableRef
        , recordAccessFn
        , hexLiteral
        , prefixOperator
        ]
        |> Random.andThen identity


intLiteral : Generator String
intLiteral =
    Random.int -999 999 |> Random.map String.fromInt


floatLiteral : Generator String
floatLiteral =
    Random.map2
        (\whole frac ->
            String.fromInt (abs whole) ++ "." ++ String.fromInt (abs frac)
        )
        (Random.int 0 999)
        (Random.int 1 99)


stringLiteral : Generator String
stringLiteral =
    Random.uniform "hello" [ "world", "foo", "bar", "", "test 123", "it's" ]
        |> Random.map (\s -> "\"" ++ escapeString s ++ "\"")


charLiteral : Generator String
charLiteral =
    Random.uniform 'a' [ 'b', 'z', '0', 'A', ' ', '!' ]
        |> Random.map (\c -> "'" ++ escapeChar c ++ "'")


unitLiteral : Generator String
unitLiteral =
    Random.constant "()"


variableRef : Generator String
variableRef =
    Identifier.lowerName


recordAccessFn : Generator String
recordAccessFn =
    Identifier.lowerName |> Random.map (\n -> "." ++ n)


hexLiteral : Generator String
hexLiteral =
    Random.int 0 4095 |> Random.map (\n -> "0x" ++ String.toUpper (Hex.toString n))


prefixOperator : Generator String
prefixOperator =
    Random.uniform "(+)" [ "(-)", "(*)", "(//)", "(++)", "(::)", "(&&)", "(||)", "(==)", "(/=)" ]


application : Int -> Generator String
application depth =
    Random.map2
        (\fn arg -> fn ++ " " ++ arg)
        Identifier.lowerName
        (generator (depth - 1))


operatorExpr : Int -> Generator String
operatorExpr depth =
    Random.map3
        (\left op right -> left ++ " " ++ op ++ " " ++ right)
        (generator (depth - 1))
        operator
        (generator (depth - 1))


operator : Generator String
operator =
    Random.uniform "+" [ "-", "*", "//", "++", "::", "&&", "||", "==", "/=", "<", ">", "<=", ">=" ]


ifExpr : Int -> Generator String
ifExpr depth =
    Random.map3
        (\cond then_ else_ ->
            "if " ++ cond ++ " then " ++ then_ ++ " else " ++ else_
        )
        (generator (depth - 1))
        (generator (depth - 1))
        (generator (depth - 1))


caseExpr : Int -> Generator String
caseExpr depth =
    let
        branch : Generator String
        branch =
            Random.map2
                (\pat body -> "        " ++ pat ++ " ->\n            " ++ body)
                (Pattern.generator 1)
                leaf
    in
    Random.map2
        (\subject branches ->
            "case " ++ subject ++ " of\n" ++ String.join "\n\n" branches
        )
        leaf
        (Random.int 1 3
            |> Random.andThen (\n -> randomList n branch)
        )


letExpr : Int -> Generator String
letExpr depth =
    let
        simpleBinding : Generator String
        simpleBinding =
            Random.map2
                (\name body -> "        " ++ name ++ " =\n            " ++ body)
                Identifier.lowerName
                leaf

        destructuringBinding : Generator String
        destructuringBinding =
            Random.map2
                (\pat body -> "        " ++ pat ++ " =\n            " ++ body)
                (Random.uniform
                    (Random.map2 (\a b -> "( " ++ a ++ ", " ++ b ++ " )") Identifier.lowerName Identifier.lowerName)
                    [ Random.map (\names -> "{ " ++ String.join ", " names ++ " }")
                        (Random.int 1 3 |> Random.andThen (\n -> randomList n Identifier.lowerName))
                    ]
                    |> Random.andThen identity
                )
                leaf

        binding : Generator String
        binding =
            Random.uniform simpleBinding [ simpleBinding, simpleBinding, destructuringBinding ]
                |> Random.andThen identity
    in
    Random.map2
        (\bindings body ->
            "let\n" ++ String.join "\n\n" bindings ++ "\n    in\n    " ++ body
        )
        (Random.int 1 3
            |> Random.andThen (\n -> randomList n binding)
        )
        (generator (depth - 1))


lambdaExpr : Int -> Generator String
lambdaExpr depth =
    Random.map2
        (\args body ->
            "\\" ++ String.join " " args ++ " -> " ++ body
        )
        (Random.int 1 3
            |> Random.andThen (\n -> randomList n Identifier.lowerName)
        )
        (generator (depth - 1))


tupleExpr : Int -> Generator String
tupleExpr depth =
    Random.int 2 3
        |> Random.andThen
            (\n -> randomList n (generator (depth - 1)))
        |> Random.map
            (\items -> "( " ++ String.join ", " items ++ " )")


listExpr : Int -> Generator String
listExpr depth =
    Random.int 0 4
        |> Random.andThen
            (\n -> randomList n (generator (depth - 1)))
        |> Random.map
            (\items -> "[ " ++ String.join ", " items ++ " ]")


recordExpr : Int -> Generator String
recordExpr depth =
    Random.int 1 4
        |> Random.andThen
            (\n ->
                randomList n
                    (Random.map2
                        (\name val -> name ++ " = " ++ val)
                        Identifier.lowerName
                        (generator (depth - 1))
                    )
            )
        |> Random.map
            (\fields -> "{ " ++ String.join ", " fields ++ " }")


negation : Int -> Generator String
negation depth =
    generator (depth - 1) |> Random.map (\e -> "-" ++ e)


parenExpr : Int -> Generator String
parenExpr depth =
    generator (depth - 1) |> Random.map (\e -> "(" ++ e ++ ")")


recordAccessExpr : Int -> Generator String
recordAccessExpr depth =
    Random.map2
        (\expr field -> expr ++ "." ++ field)
        Identifier.lowerName
        Identifier.lowerName


recordUpdateExpr : Int -> Generator String
recordUpdateExpr depth =
    Random.map2
        (\name fields ->
            "{ " ++ name ++ " | " ++ String.join ", " fields ++ " }"
        )
        Identifier.lowerName
        (Random.int 1 3
            |> Random.andThen
                (\n ->
                    randomList n
                        (Random.map2
                            (\fname val -> fname ++ " = " ++ val)
                            Identifier.lowerName
                            leaf
                        )
                )
        )



-- HELPERS


escapeString : String -> String
escapeString s =
    s
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> String.replace "\n" "\\n"


escapeChar : Char -> String
escapeChar c =
    case c of
        '\'' ->
            "\\'"

        '\\' ->
            "\\\\"

        _ ->
            String.fromChar c


randomList : Int -> Generator a -> Generator (List a)
randomList n gen =
    if n <= 0 then
        Random.constant []

    else
        Random.map2 (::) gen (randomList (n - 1) gen)
