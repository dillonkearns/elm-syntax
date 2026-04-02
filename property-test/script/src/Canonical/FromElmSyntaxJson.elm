module Canonical.FromElmSyntaxJson exposing (decoder)

{-| Decode elm-syntax's JSON AST output into canonical types.

elm-syntax JSON uses:

  - `"type"` field for tagged unions (via encodeTyped)
  - `"range"` as 4-element int array [startRow, startCol, endRow, endCol]
  - `"value"` for the Node wrapper's inner value
  - Separate `signature` and `declaration` for function definitions
  - `OperatorApplication` as distinct type from `Application`
  - `FunctionOrValue` for both functions and constructors
  - `var` for variable patterns
  - `allPattern` for wildcards

-}

import Canonical exposing (..)
import Json.Decode as D exposing (Decoder)


decoder : Decoder CanonicalFile
decoder =
    D.field "ast"
        (D.map2 CanonicalFile
            (D.field "moduleDefinition" (nodeDecoder moduleNameDecoder))
            (D.field "declarations" (D.list (nodeDecoder declarationDecoder)))
        )


moduleNameDecoder : Decoder (List String)
moduleNameDecoder =
    -- Module is encoded as { "type": "normal"|"port"|"effect", "<type>": { "moduleName": Node [String] } }
    D.field "type" D.string
        |> D.andThen
            (\modType ->
                D.field modType
                    (D.field "moduleName" (nodeDecoder (D.list D.string)))
            )


{-| Unwrap a Node: { "range": [...], "value": ... }
-}
nodeDecoder : Decoder a -> Decoder a
nodeDecoder inner =
    D.field "value" inner


declarationDecoder : Decoder CanonicalDeclaration
declarationDecoder =
    D.field "type" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "function" ->
                        D.field "function" functionDecoder

                    "typeAlias" ->
                        D.field "typeAlias" typeAliasDecoder_

                    "typedecl" ->
                        D.field "typedecl" customTypeDecoder_

                    "port" ->
                        D.field "port" portDecoder_

                    "destructuring" ->
                        D.field "destructuring" destructuringDecoder

                    other ->
                        D.fail ("Unknown declaration type: " ++ other)
            )


functionDecoder : Decoder CanonicalDeclaration
functionDecoder =
    D.map3
        (\maybeSig name impl ->
            let
                typeAnn =
                    maybeSig
                        |> Maybe.map
                            (\sig ->
                                sig
                            )
            in
            CanonicalFunction
                { name = name
                , typeAnnotation = typeAnn
                , arguments = impl.arguments
                , body = impl.body
                }
        )
        (D.field "signature"
            (D.nullable
                (nodeDecoder
                    (D.field "typeAnnotation" (nodeDecoder typeAnnotationDecoder_))
                )
            )
        )
        (D.field "declaration" (nodeDecoder (D.field "name" (nodeDecoder D.string))))
        (D.field "declaration" (nodeDecoder functionImplDecoder))


type alias FunctionImpl =
    { arguments : List CanonicalPattern
    , body : CanonicalExpression
    }


functionImplDecoder : Decoder FunctionImpl
functionImplDecoder =
    D.map2 FunctionImpl
        (D.field "arguments" (D.list (nodeDecoder patternDecoder_)))
        (D.field "expression" (nodeDecoder expressionDecoder_))


typeAliasDecoder_ : Decoder CanonicalDeclaration
typeAliasDecoder_ =
    D.map3
        (\name generics def ->
            CanonicalTypeAlias
                { name = name
                , generics = generics
                , definition = def
                }
        )
        (D.field "name" (nodeDecoder D.string))
        (D.field "generics" (D.list (nodeDecoder D.string)))
        (D.field "typeAnnotation" (nodeDecoder typeAnnotationDecoder_))


customTypeDecoder_ : Decoder CanonicalDeclaration
customTypeDecoder_ =
    D.map3
        (\name generics ctors ->
            CanonicalCustomType
                { name = name
                , generics = generics
                , constructors = ctors
                }
        )
        (D.field "name" (nodeDecoder D.string))
        (D.field "generics" (D.list (nodeDecoder D.string)))
        (D.field "constructors" (D.list (nodeDecoder constructorDecoder_)))


constructorDecoder_ : Decoder CanonicalConstructor
constructorDecoder_ =
    D.map2 CanonicalConstructor
        (D.field "name" (nodeDecoder D.string))
        (D.field "arguments" (D.list (nodeDecoder typeAnnotationDecoder_)))


portDecoder_ : Decoder CanonicalDeclaration
portDecoder_ =
    D.map2
        (\name typeAnn ->
            CanonicalPort
                { name = name
                , typeAnnotation = typeAnn
                }
        )
        (D.field "name" (nodeDecoder D.string))
        (D.field "typeAnnotation" (nodeDecoder typeAnnotationDecoder_))


destructuringDecoder : Decoder CanonicalDeclaration
destructuringDecoder =
    D.map2 CanonicalDestructuring
        (D.field "pattern" (nodeDecoder patternDecoder_))
        (D.field "expression" (nodeDecoder expressionDecoder_))



-- EXPRESSIONS


expressionDecoder_ : Decoder CanonicalExpression
expressionDecoder_ =
    D.field "type" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "unit" ->
                        D.succeed CUnit

                    "application" ->
                        D.field "application"
                            (D.list (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
                            |> D.map CApplication

                    "operatorapplication" ->
                        D.field "operatorapplication" operatorApplicationDecoder

                    "functionOrValue" ->
                        D.field "functionOrValue"
                            (D.map2 CFunctionOrValue
                                (D.field "moduleName" (D.list D.string))
                                (D.field "name" D.string)
                            )

                    "ifBlock" ->
                        D.field "ifBlock"
                            (D.map3 CIfBlock
                                (D.field "clause" (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
                                (D.field "then" (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
                                (D.field "else" (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
                            )

                    "prefixoperator" ->
                        D.field "prefixoperator" D.string |> D.map CPrefixOperator

                    "operator" ->
                        D.field "operator" D.string |> D.map CPrefixOperator

                    "integer" ->
                        D.field "integer" D.int |> D.map CInt

                    "hex" ->
                        D.field "hex" D.int |> D.map CInt

                    "floatable" ->
                        D.field "floatable" D.float |> D.map CFloat

                    "negation" ->
                        D.field "negation"
                            (nodeDecoder (D.lazy (\_ -> expressionDecoder_)))
                            |> D.map CNegation

                    "literal" ->
                        D.field "literal" D.string |> D.map CStringLiteral

                    "charLiteral" ->
                        D.field "charLiteral" D.string
                            |> D.map
                                (\s ->
                                    case String.uncons s of
                                        Just ( c, _ ) ->
                                            CCharLiteral c

                                        Nothing ->
                                            CCharLiteral ' '
                                )

                    "tupled" ->
                        D.field "tupled"
                            (D.list (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
                            |> D.map CTuple

                    "parenthesized" ->
                        -- Unwrap parenthesized expression
                        D.field "parenthesized"
                            (nodeDecoder (D.lazy (\_ -> expressionDecoder_)))

                    "let" ->
                        D.field "let"
                            (D.map2 CLet
                                (D.field "declarations"
                                    (D.list (nodeDecoder (D.lazy (\_ -> letDeclarationDecoder_))))
                                )
                                (D.field "expression"
                                    (nodeDecoder (D.lazy (\_ -> expressionDecoder_)))
                                )
                            )

                    "case" ->
                        D.field "case"
                            (D.map2 CCase
                                (D.field "expression"
                                    (nodeDecoder (D.lazy (\_ -> expressionDecoder_)))
                                )
                                (D.field "cases"
                                    (D.list
                                        (D.map2 Tuple.pair
                                            (D.field "pattern" (nodeDecoder (D.lazy (\_ -> patternDecoder_))))
                                            (D.field "expression" (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
                                        )
                                    )
                                )
                            )

                    "lambda" ->
                        D.field "lambda"
                            (D.map2 CLambda
                                (D.field "patterns"
                                    (D.list (nodeDecoder (D.lazy (\_ -> patternDecoder_))))
                                )
                                (D.field "expression"
                                    (nodeDecoder (D.lazy (\_ -> expressionDecoder_)))
                                )
                            )

                    "recordExpr" ->
                        D.field "recordExpr"
                            (D.list
                                (nodeDecoder
                                    (D.map2 Tuple.pair
                                        (D.index 0 (nodeDecoder D.string))
                                        (D.index 1 (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
                                    )
                                )
                            )
                            |> D.map CRecordExpr

                    "list" ->
                        D.field "list"
                            (D.list (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
                            |> D.map CList

                    "recordAccess" ->
                        D.field "recordAccess"
                            (D.map2 CRecordAccess
                                (D.index 0 (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
                                (D.index 1 (nodeDecoder D.string))
                            )

                    "recordAccessFunction" ->
                        D.field "recordAccessFunction" D.string |> D.map CRecordAccessFunction

                    "recordUpdate" ->
                        D.field "recordUpdate"
                            (D.map2 CRecordUpdate
                                (D.field "name" (nodeDecoder D.string))
                                (D.field "updates"
                                    (D.list
                                        (nodeDecoder
                                            (D.map2 Tuple.pair
                                                (D.index 0 (nodeDecoder D.string))
                                                (D.index 1 (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
                                            )
                                        )
                                    )
                                )
                            )

                    "glsl" ->
                        D.field "glsl" D.string |> D.map CGlsl

                    other ->
                        D.fail ("Unknown expression type: " ++ other)
            )


operatorApplicationDecoder : Decoder CanonicalExpression
operatorApplicationDecoder =
    D.map3
        (\op left right ->
            CApplication [ CFunctionOrValue [] op, left, right ]
        )
        (D.field "operator" D.string)
        (D.field "left" (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
        (D.field "right" (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))


letDeclarationDecoder_ : Decoder CanonicalLetDeclaration
letDeclarationDecoder_ =
    D.field "type" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "function" ->
                        D.field "function"
                            (D.map3
                                (\maybeSig name impl ->
                                    CLetFunction
                                        { name = name
                                        , typeAnnotation =
                                            maybeSig
                                        , arguments = impl.arguments
                                        , body = impl.body
                                        }
                                )
                                (D.field "signature"
                                    (D.nullable
                                        (nodeDecoder
                                            (D.field "typeAnnotation" (nodeDecoder typeAnnotationDecoder_))
                                        )
                                    )
                                )
                                (D.field "declaration" (nodeDecoder (D.field "name" (nodeDecoder D.string))))
                                (D.field "declaration" (nodeDecoder functionImplDecoder))
                            )

                    "destructuring" ->
                        D.field "destructuring"
                            (D.map2 CLetDestructuring
                                (D.field "pattern" (nodeDecoder (D.lazy (\_ -> patternDecoder_))))
                                (D.field "expression" (nodeDecoder (D.lazy (\_ -> expressionDecoder_))))
                            )

                    other ->
                        D.fail ("Unknown let declaration type: " ++ other)
            )



-- PATTERNS


patternDecoder_ : Decoder CanonicalPattern
patternDecoder_ =
    D.field "type" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "all" ->
                        D.succeed CAllPattern

                    "unit" ->
                        D.succeed CUnitPattern

                    "char" ->
                        D.field "char" (D.field "value" D.string)
                            |> D.map
                                (\s ->
                                    case String.uncons s of
                                        Just ( c, _ ) ->
                                            CCharPattern c

                                        Nothing ->
                                            CCharPattern ' '
                                )

                    "string" ->
                        D.field "string" (D.field "value" D.string) |> D.map CStringPattern

                    "int" ->
                        D.field "int" (D.field "value" D.int) |> D.map CIntPattern

                    "hex" ->
                        D.field "hex" (D.field "value" D.int) |> D.map CIntPattern

                    "float" ->
                        D.field "float" (D.field "value" D.float) |> D.map CFloatPattern

                    "tuple" ->
                        D.field "tuple" (D.field "value"
                            (D.list (nodeDecoder (D.lazy (\_ -> patternDecoder_)))))
                            |> D.map CTuplePattern

                    "record" ->
                        D.field "record" (D.field "value"
                            (D.list (nodeDecoder D.string)))
                            |> D.map (List.sort >> CRecordPattern)

                    "uncons" ->
                        D.field "uncons"
                            (D.map2 CConsPattern
                                (D.field "left" (nodeDecoder (D.lazy (\_ -> patternDecoder_))))
                                (D.field "right" (nodeDecoder (D.lazy (\_ -> patternDecoder_))))
                            )

                    "list" ->
                        D.field "list" (D.field "value"
                            (D.list (nodeDecoder (D.lazy (\_ -> patternDecoder_)))))
                            |> D.map CListPattern

                    "var" ->
                        D.field "var" (D.field "value" D.string) |> D.map CVarPattern

                    "named" ->
                        D.field "named"
                            (D.map3 CNamedPattern
                                (D.field "qualified" (D.field "moduleName" (D.list D.string)))
                                (D.field "qualified" (D.field "name" D.string))
                                (D.field "patterns" (D.list (nodeDecoder (D.lazy (\_ -> patternDecoder_)))))
                            )

                    "as" ->
                        D.field "as"
                            (D.map2 CAsPattern
                                (D.field "pattern" (nodeDecoder (D.lazy (\_ -> patternDecoder_))))
                                (D.field "name" (nodeDecoder D.string))
                            )

                    "parentisized" ->
                        D.field "parentisized"
                            (D.field "value" (nodeDecoder (D.lazy (\_ -> patternDecoder_))))

                    other ->
                        D.fail ("Unknown pattern type: " ++ other)
            )



-- TYPE ANNOTATIONS


typeAnnotationDecoder_ : Decoder CanonicalTypeAnnotation
typeAnnotationDecoder_ =
    D.field "type" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "generic" ->
                        D.field "generic" (D.field "value" D.string) |> D.map CGenericType

                    "typed" ->
                        D.field "typed"
                            (D.map3 CTyped
                                (D.field "moduleNameAndName"
                                    (nodeDecoder (D.field "moduleName" (D.list D.string)))
                                )
                                (D.field "moduleNameAndName"
                                    (nodeDecoder (D.field "name" D.string))
                                )
                                (D.field "args"
                                    (D.list (nodeDecoder (D.lazy (\_ -> typeAnnotationDecoder_))))
                                )
                            )

                    "unit" ->
                        D.succeed CUnitType

                    "tupled" ->
                        D.field "tupled"
                            (D.field "values"
                                (D.list (nodeDecoder (D.lazy (\_ -> typeAnnotationDecoder_))))
                            )
                            |> D.map CTupleType

                    "record" ->
                        D.field "record"
                            (D.field "value"
                                (D.list
                                    (nodeDecoder
                                        (D.map2 Tuple.pair
                                            (D.field "name" (nodeDecoder D.string))
                                            (D.field "typeAnnotation" (nodeDecoder (D.lazy (\_ -> typeAnnotationDecoder_))))
                                        )
                                    )
                                )
                            )
                            |> D.map CRecordType

                    "genericRecord" ->
                        D.field "genericRecord"
                            (D.map2 CGenericRecordType
                                (D.field "name" (nodeDecoder D.string))
                                (D.field "values"
                                    (nodeDecoder
                                        (D.list
                                            (nodeDecoder
                                                (D.map2 Tuple.pair
                                                    (D.field "name" (nodeDecoder D.string))
                                                    (D.field "typeAnnotation" (nodeDecoder (D.lazy (\_ -> typeAnnotationDecoder_))))
                                                )
                                            )
                                        )
                                    )
                                )
                            )

                    "function" ->
                        D.field "function"
                            (D.map2 CFunctionType
                                (D.field "left" (nodeDecoder (D.lazy (\_ -> typeAnnotationDecoder_))))
                                (D.field "right" (nodeDecoder (D.lazy (\_ -> typeAnnotationDecoder_))))
                            )

                    other ->
                        D.fail ("Unknown type annotation type: " ++ other)
            )
