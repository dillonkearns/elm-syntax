module Generate.Identifier exposing (lowerName, upperName)

{-| Generate valid Elm identifiers, avoiding reserved keywords.
-}

import Random exposing (Generator)


{-| Generate a valid lowercase Elm identifier (for variables, functions).
-}
lowerName : Generator String
lowerName =
    Random.uniform "alpha" lowerNames
        |> Random.andThen
            (\name ->
                if isKeyword name then
                    -- Append a suffix to avoid keywords
                    Random.int 1 99
                        |> Random.map (\n -> name ++ String.fromInt n)

                else
                    Random.constant name
            )


{-| Generate a valid uppercase Elm identifier (for types, constructors, modules).
-}
upperName : Generator String
upperName =
    Random.uniform "Alpha" upperNames


lowerNames : List String
lowerNames =
    [ "foo"
    , "bar"
    , "baz"
    , "x"
    , "y"
    , "z"
    , "name"
    , "value"
    , "count"
    , "result"
    , "msg"
    , "model"
    , "state"
    , "acc"
    , "item"
    , "head"
    , "tail"
    , "key"
    , "val"
    , "a"
    , "b"
    , "c"
    , "n"
    , "m"
    , "f"
    , "g"
    , "xs"
    , "ys"
    ]


upperNames : List String
upperNames =
    [ "Foo"
    , "Bar"
    , "Baz"
    , "MyType"
    , "Result"
    , "Value"
    , "Node"
    , "Item"
    , "State"
    , "Action"
    , "Variant"
    , "Alpha"
    , "Beta"
    , "Gamma"
    ]


isKeyword : String -> Bool
isKeyword name =
    List.member name keywords


keywords : List String
keywords =
    [ "if"
    , "then"
    , "else"
    , "case"
    , "of"
    , "let"
    , "in"
    , "type"
    , "module"
    , "where"
    , "import"
    , "exposing"
    , "as"
    , "port"
    ]
