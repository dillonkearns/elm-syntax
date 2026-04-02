module Canonical.Diff exposing (DiffItem, diff, formatDiff)

{-| Structural diff of two CanonicalFiles.
Produces path-annotated differences for debugging.
-}

import Canonical exposing (..)
import Canonical.Encode as Encode
import Json.Encode as E


type alias DiffItem =
    { path : String
    , elmSyntax : String
    , elmFormat : String
    }


diff : CanonicalFile -> CanonicalFile -> List DiffItem
diff a b =
    diffModuleName "" a.moduleName b.moduleName
        ++ diffDeclarationLists "declarations" a.declarations b.declarations


diffModuleName : String -> List String -> List String -> List DiffItem
diffModuleName path a b =
    if a == b then
        []

    else
        [ { path = path ++ "moduleName"
          , elmSyntax = String.join "." a
          , elmFormat = String.join "." b
          }
        ]


diffDeclarationLists : String -> List CanonicalDeclaration -> List CanonicalDeclaration -> List DiffItem
diffDeclarationLists path a b =
    if List.length a /= List.length b then
        [ { path = path ++ ".length"
          , elmSyntax = String.fromInt (List.length a)
          , elmFormat = String.fromInt (List.length b)
          }
        ]

    else
        List.indexedMap
            (\i ( da, db ) ->
                diffDeclaration (path ++ "[" ++ String.fromInt i ++ "]") da db
            )
            (List.map2 Tuple.pair a b)
            |> List.concat


diffDeclaration : String -> CanonicalDeclaration -> CanonicalDeclaration -> List DiffItem
diffDeclaration path a b =
    case ( a, b ) of
        ( CanonicalFunction fa, CanonicalFunction fb ) ->
            diffFunctionDef path fa fb

        ( CanonicalTypeAlias ta, CanonicalTypeAlias tb ) ->
            diffString (path ++ ".name") ta.name tb.name
                ++ diffStringLists (path ++ ".generics") ta.generics tb.generics
                ++ diffTypeAnnotation (path ++ ".definition") ta.definition tb.definition

        ( CanonicalCustomType ca, CanonicalCustomType cb ) ->
            diffString (path ++ ".name") ca.name cb.name
                ++ diffStringLists (path ++ ".generics") ca.generics cb.generics
                ++ diffConstructorLists (path ++ ".constructors") ca.constructors cb.constructors

        ( CanonicalPort pa, CanonicalPort pb ) ->
            diffString (path ++ ".name") pa.name pb.name
                ++ diffTypeAnnotation (path ++ ".typeAnnotation") pa.typeAnnotation pb.typeAnnotation

        ( CanonicalDestructuring pa ea, CanonicalDestructuring pb eb ) ->
            diffPattern (path ++ ".pattern") pa pb
                ++ diffExpression (path ++ ".expression") ea eb

        _ ->
            [ { path = path ++ ".kind"
              , elmSyntax = declKind a
              , elmFormat = declKind b
              }
            ]


diffFunctionDef : String -> CanonicalFunctionDef -> CanonicalFunctionDef -> List DiffItem
diffFunctionDef path a b =
    diffString (path ++ ".name") a.name b.name
        ++ diffPatternLists (path ++ ".arguments") a.arguments b.arguments
        ++ diffExpression (path ++ ".body") a.body b.body


diffExpression : String -> CanonicalExpression -> CanonicalExpression -> List DiffItem
diffExpression path a b =
    if exprEq a b then
        []

    else
        [ { path = path
          , elmSyntax = E.encode 0 (Encode.encodeFile { moduleName = [], declarations = [ CanonicalFunction { name = "_", typeAnnotation = Nothing, arguments = [], body = a } ] })
          , elmFormat = E.encode 0 (Encode.encodeFile { moduleName = [], declarations = [ CanonicalFunction { name = "_", typeAnnotation = Nothing, arguments = [], body = b } ] })
          }
        ]


diffPattern : String -> CanonicalPattern -> CanonicalPattern -> List DiffItem
diffPattern path a b =
    if patEq a b then
        []

    else
        [ { path = path
          , elmSyntax = showPattern a
          , elmFormat = showPattern b
          }
        ]


diffTypeAnnotation : String -> CanonicalTypeAnnotation -> CanonicalTypeAnnotation -> List DiffItem
diffTypeAnnotation path a b =
    if typeEq a b then
        []

    else
        [ { path = path
          , elmSyntax = showType a
          , elmFormat = showType b
          }
        ]


diffPatternLists : String -> List CanonicalPattern -> List CanonicalPattern -> List DiffItem
diffPatternLists path a b =
    if List.length a /= List.length b then
        [ { path = path ++ ".length"
          , elmSyntax = String.fromInt (List.length a)
          , elmFormat = String.fromInt (List.length b)
          }
        ]

    else
        List.indexedMap
            (\i ( pa, pb ) ->
                diffPattern (path ++ "[" ++ String.fromInt i ++ "]") pa pb
            )
            (List.map2 Tuple.pair a b)
            |> List.concat


diffConstructorLists : String -> List CanonicalConstructor -> List CanonicalConstructor -> List DiffItem
diffConstructorLists path a b =
    if List.length a /= List.length b then
        [ { path = path ++ ".length"
          , elmSyntax = String.fromInt (List.length a)
          , elmFormat = String.fromInt (List.length b)
          }
        ]

    else
        List.indexedMap
            (\i ( ca, cb ) ->
                let
                    p =
                        path ++ "[" ++ String.fromInt i ++ "]"
                in
                diffString (p ++ ".name") ca.name cb.name
                    ++ diffTypeAnnotationLists (p ++ ".arguments") ca.arguments cb.arguments
            )
            (List.map2 Tuple.pair a b)
            |> List.concat


diffTypeAnnotationLists : String -> List CanonicalTypeAnnotation -> List CanonicalTypeAnnotation -> List DiffItem
diffTypeAnnotationLists path a b =
    if List.length a /= List.length b then
        [ { path = path ++ ".length"
          , elmSyntax = String.fromInt (List.length a)
          , elmFormat = String.fromInt (List.length b)
          }
        ]

    else
        List.indexedMap
            (\i ( ta, tb ) ->
                diffTypeAnnotation (path ++ "[" ++ String.fromInt i ++ "]") ta tb
            )
            (List.map2 Tuple.pair a b)
            |> List.concat


diffString : String -> String -> String -> List DiffItem
diffString path a b =
    if a == b then
        []

    else
        [ { path = path, elmSyntax = a, elmFormat = b } ]


diffStringLists : String -> List String -> List String -> List DiffItem
diffStringLists path a b =
    if a == b then
        []

    else
        [ { path = path
          , elmSyntax = String.join ", " a
          , elmFormat = String.join ", " b
          }
        ]



-- EQUALITY CHECKS (structural, ignoring ranges)


exprEq : CanonicalExpression -> CanonicalExpression -> Bool
exprEq a b =
    a == b


patEq : CanonicalPattern -> CanonicalPattern -> Bool
patEq a b =
    a == b


typeEq : CanonicalTypeAnnotation -> CanonicalTypeAnnotation -> Bool
typeEq a b =
    a == b



-- DISPLAY HELPERS


declKind : CanonicalDeclaration -> String
declKind d =
    case d of
        CanonicalFunction _ ->
            "function"

        CanonicalTypeAlias _ ->
            "typeAlias"

        CanonicalCustomType _ ->
            "customType"

        CanonicalPort _ ->
            "port"

        CanonicalDestructuring _ _ ->
            "destructuring"


showPattern : CanonicalPattern -> String
showPattern p =
    E.encode 0
        (Encode.encodeFile
            { moduleName = []
            , declarations =
                [ CanonicalDestructuring p CUnit ]
            }
        )


showType : CanonicalTypeAnnotation -> String
showType t =
    E.encode 0
        (Encode.encodeFile
            { moduleName = []
            , declarations =
                [ CanonicalTypeAlias { name = "_", generics = [], definition = t } ]
            }
        )


formatDiff : List DiffItem -> String
formatDiff items =
    items
        |> List.map
            (\item ->
                "  Path: " ++ item.path ++ "\n    elm-syntax: " ++ item.elmSyntax ++ "\n    elm-format: " ++ item.elmFormat
            )
        |> String.join "\n"
