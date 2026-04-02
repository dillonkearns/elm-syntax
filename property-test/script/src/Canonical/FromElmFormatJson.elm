module Canonical.FromElmFormatJson exposing (decoder)

{-| Decode elm-format's --json output into canonical AST types.

elm-format JSON uses:

  - `"tag"` field for discriminated unions
  - `"sourceLocation"` for ranges (ignored)
  - `"display"` for formatting hints (ignored)
  - `Definition` merges type annotations into parameters
  - `FunctionApplication` with `showAsInfix` for infix operators
  - `VariableReference` / `ExternalReference` for variable access
  - `VariableDefinition` for variable patterns
  - `AnythingPattern` for wildcard

-}

import Canonical exposing (..)
import Json.Decode as D exposing (Decoder)


decoder : Decoder CanonicalFile
decoder =
    D.map2 CanonicalFile
        (D.field "moduleName" (D.string |> D.map (\s -> String.split "." s)))
        (D.field "body" (D.list declarationDecoder))


declarationDecoder : Decoder CanonicalDeclaration
declarationDecoder =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "Definition" ->
                        definitionDecoder

                    "TypeAlias" ->
                        typeAliasDecoder

                    "CustomType" ->
                        customTypeDecoder

                    "PortDefinition" ->
                        portDecoder

                    "TODO" ->
                        -- elm-format produces TODO nodes for port declarations
                        -- and other unsupported constructs. We skip these.
                        D.fail "TODO node - skipping"

                    other ->
                        D.fail ("Unknown declaration tag: " ++ other)
            )


definitionDecoder : Decoder CanonicalDeclaration
definitionDecoder =
    D.map4
        (\name typeAnn args body ->
            CanonicalFunction
                { name = name
                , typeAnnotation = typeAnn
                , arguments = args
                , body = body
                }
        )
        (D.field "name" D.string)
        (D.field "returnType" (D.nullable typeAnnotationDecoder))
        (D.field "parameters"
            (D.list (D.field "pattern" patternDecoder))
        )
        (D.field "expression" expressionDecoder)


typeAliasDecoder : Decoder CanonicalDeclaration
typeAliasDecoder =
    D.map3
        (\name params typeDef ->
            CanonicalTypeAlias
                { name = name
                , generics = params
                , definition = typeDef
                }
        )
        (D.field "name" D.string)
        (D.field "parameters" (D.list D.string))
        (D.field "type" typeAnnotationDecoder)


customTypeDecoder : Decoder CanonicalDeclaration
customTypeDecoder =
    D.map3
        (\name params variants ->
            CanonicalCustomType
                { name = name
                , generics = params
                , constructors = variants
                }
        )
        (D.field "name" D.string)
        (D.field "parameters" (D.list D.string))
        (D.field "variants" (D.list constructorDecoder))


constructorDecoder : Decoder CanonicalConstructor
constructorDecoder =
    D.map2 CanonicalConstructor
        (D.field "name" D.string)
        (D.field "parameterTypes" (D.list typeAnnotationDecoder))


portDecoder : Decoder CanonicalDeclaration
portDecoder =
    D.map2
        (\name typeAnn ->
            CanonicalPort
                { name = name
                , typeAnnotation = typeAnn
                }
        )
        (D.field "name" D.string)
        (D.field "type" typeAnnotationDecoder)



-- EXPRESSIONS


expressionDecoder : Decoder CanonicalExpression
expressionDecoder =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "UnitLiteral" ->
                        D.succeed CUnit

                    "IntLiteral" ->
                        D.field "value" D.int |> D.map CInt

                    "FloatLiteral" ->
                        D.field "value" D.float |> D.map CFloat

                    "StringLiteral" ->
                        D.field "value" D.string |> D.map CStringLiteral

                    "CharLiteral" ->
                        D.field "value" D.string
                            |> D.map
                                (\s ->
                                    case String.uncons s of
                                        Just ( c, _ ) ->
                                            CCharLiteral c

                                        Nothing ->
                                            CCharLiteral ' '
                                )

                    "VariableReference" ->
                        D.field "name" D.string
                            |> D.map (CFunctionOrValue [])

                    "ExternalReference" ->
                        D.map2
                            (\mod name ->
                                let
                                    modParts =
                                        String.split "." mod
                                in
                                -- Normalize: strip implicit module (e.g., Basics.True -> True)
                                if modParts == [ "Basics" ] then
                                    CFunctionOrValue [] name

                                else
                                    CFunctionOrValue modParts name
                            )
                            (D.field "module" D.string)
                            (D.field "name" D.string)

                    "ConstructorReference" ->
                        D.field "name" D.string
                            |> D.map (CFunctionOrValue [])

                    "FunctionApplication" ->
                        functionApplicationDecoder

                    "UnaryOperator" ->
                        -- elm-format sometimes uses "term" and sometimes doesn't include
                        -- the operand when it's a bare operator reference
                        D.oneOf
                            [ D.field "term" (D.lazy (\_ -> expressionDecoder))
                                |> D.map CNegation
                            , D.succeed (CPrefixOperator "-")
                            ]

                    "NegateExpression" ->
                        D.field "term" (D.lazy (\_ -> expressionDecoder))
                            |> D.map CNegation

                    "IfExpression" ->
                        D.map3 CIfBlock
                            (D.field "if" (D.lazy (\_ -> expressionDecoder)))
                            (D.field "then" (D.lazy (\_ -> expressionDecoder)))
                            (D.field "else" (D.lazy (\_ -> expressionDecoder)))

                    "LetExpression" ->
                        D.map2 CLet
                            (D.field "declarations" (D.list (D.lazy (\_ -> letDeclarationDecoder))))
                            (D.field "body" (D.lazy (\_ -> expressionDecoder)))

                    "CaseExpression" ->
                        D.map2 CCase
                            (D.field "subject" (D.lazy (\_ -> expressionDecoder)))
                            (D.field "branches"
                                (D.list
                                    (D.map2 Tuple.pair
                                        (D.field "pattern" (D.lazy (\_ -> patternDecoder)))
                                        (D.field "body" (D.lazy (\_ -> expressionDecoder)))
                                    )
                                )
                            )

                    "LambdaExpression" ->
                        D.map2 CLambda
                            (D.field "patterns" (D.list (D.lazy (\_ -> patternDecoder))))
                            (D.field "body" (D.lazy (\_ -> expressionDecoder)))

                    "AnonymousFunction" ->
                        D.map2 CLambda
                            (D.field "parameters" (D.list (D.lazy (\_ -> patternDecoder))))
                            (D.field "body" (D.lazy (\_ -> expressionDecoder)))

                    "TupleExpression" ->
                        D.field "terms" (D.list (D.lazy (\_ -> expressionDecoder)))
                            |> D.map CTuple

                    "TupleLiteral" ->
                        D.field "terms" (D.list (D.lazy (\_ -> expressionDecoder)))
                            |> D.map CTuple

                    "ListExpression" ->
                        D.field "terms" (D.list (D.lazy (\_ -> expressionDecoder)))
                            |> D.map CList

                    "ListLiteral" ->
                        D.field "terms" (D.list (D.lazy (\_ -> expressionDecoder)))
                            |> D.map CList

                    "EmptyListLiteral" ->
                        D.succeed (CList [])

                    "RecordExpression" ->
                        recordExprDecoder

                    "RecordLiteral" ->
                        recordExprDecoder

                    "RecordUpdateExpression" ->
                        recordUpdateDecoder

                    "RecordUpdate" ->
                        recordUpdateDecoder

                    "RecordAccessFunction" ->
                        D.field "field" D.string
                            |> D.map CRecordAccessFunction

                    "RecordAccess" ->
                        D.map2 CRecordAccess
                            (D.field "record" (D.lazy (\_ -> expressionDecoder)))
                            (D.field "field" D.string)

                    "GLSLExpression" ->
                        D.field "src" D.string |> D.map CGlsl

                    "PrefixOperator" ->
                        D.field "operator" D.string |> D.map CPrefixOperator

                    other ->
                        D.fail ("Unknown expression tag: " ++ other)
            )


functionApplicationDecoder : Decoder CanonicalExpression
functionApplicationDecoder =
    D.map2
        (\fn args ->
            CApplication (fn :: args)
        )
        (D.field "function" (D.lazy (\_ -> expressionDecoder)))
        (D.field "arguments" (D.list (D.lazy (\_ -> expressionDecoder))))


letDeclarationDecoder : Decoder CanonicalLetDeclaration
letDeclarationDecoder =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "Definition" ->
                        D.map4
                            (\name typeAnn args body ->
                                CLetFunction
                                    { name = name
                                    , typeAnnotation = typeAnn
                                    , arguments = args
                                    , body = body
                                    }
                            )
                            (D.field "name" D.string)
                            (D.field "returnType" (D.nullable typeAnnotationDecoder))
                            (D.field "parameters" (D.list (D.field "pattern" patternDecoder)))
                            (D.field "expression" (D.lazy (\_ -> expressionDecoder)))

                    "Destructuring" ->
                        D.map2 CLetDestructuring
                            (D.field "pattern" (D.lazy (\_ -> patternDecoder)))
                            (D.field "expression" (D.lazy (\_ -> expressionDecoder)))

                    other ->
                        D.fail ("Unknown let declaration tag: " ++ other)
            )


recordExprDecoder : Decoder CanonicalExpression
recordExprDecoder =
    D.field "fields"
        (D.keyValuePairs (D.lazy (\_ -> expressionDecoder))
            |> D.map CRecordExpr
        )


constructorRefDecoder : Decoder ( List String, String )
constructorRefDecoder =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "ConstructorReference" ->
                        D.field "name" D.string
                            |> D.map (\n -> ( [], n ))

                    "ExternalReference" ->
                        D.map2
                            (\mod name ->
                                let
                                    modParts =
                                        String.split "." mod
                                in
                                if modParts == [ "Basics" ] then
                                    ( [], name )

                                else
                                    ( modParts, name )
                            )
                            (D.field "module" D.string)
                            (D.field "identifier" D.string)

                    other ->
                        D.fail ("Unknown constructor ref tag: " ++ other)
            )


recordUpdateDecoder : Decoder CanonicalExpression
recordUpdateDecoder =
    D.map2 CRecordUpdate
        (D.field "base" D.string)
        (D.field "fields"
            (D.keyValuePairs (D.lazy (\_ -> expressionDecoder)))
        )



-- PATTERNS


patternDecoder : Decoder CanonicalPattern
patternDecoder =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "AnythingPattern" ->
                        D.succeed CAllPattern

                    "UnitPattern" ->
                        D.succeed CUnitPattern

                    "VariableDefinition" ->
                        D.field "name" D.string |> D.map CVarPattern

                    "LiteralPattern" ->
                        D.field "value" D.string |> D.map CStringPattern

                    "CharLiteralPattern" ->
                        D.field "value" D.string
                            |> D.map
                                (\s ->
                                    case String.uncons s of
                                        Just ( c, _ ) ->
                                            CCharPattern c

                                        Nothing ->
                                            CCharPattern ' '
                                )

                    "IntLiteralPattern" ->
                        D.field "value" D.int |> D.map CIntPattern

                    "IntLiteral" ->
                        D.field "value" D.int |> D.map CIntPattern

                    "FloatLiteral" ->
                        D.field "value" D.float |> D.map CFloatPattern

                    "TuplePattern" ->
                        D.field "terms" (D.list (D.lazy (\_ -> patternDecoder)))
                            |> D.map CTuplePattern

                    "RecordPattern" ->
                        D.field "fields" (D.list D.string)
                            |> D.map (List.sort >> CRecordPattern)

                    "DataPattern" ->
                        D.map2
                            (\( mod, name ) args -> CNamedPattern mod name args)
                            (D.field "constructor" constructorRefDecoder)
                            (D.field "arguments" (D.list (D.lazy (\_ -> patternDecoder))))

                    "ConsPattern" ->
                        D.map2 CConsPattern
                            (D.field "head" (D.lazy (\_ -> patternDecoder)))
                            (D.field "tail" (D.lazy (\_ -> patternDecoder)))

                    "ListPattern" ->
                        D.field "terms" (D.list (D.lazy (\_ -> patternDecoder)))
                            |> D.map CListPattern

                    "AsPattern" ->
                        D.map2 CAsPattern
                            (D.field "pattern" (D.lazy (\_ -> patternDecoder)))
                            (D.field "name" D.string)

                    "StringLiteral" ->
                        -- elm-format uses StringLiteral for string patterns in case branches
                        D.field "value" D.string |> D.map CStringPattern

                    other ->
                        D.fail ("Unknown pattern tag: " ++ other)
            )



-- TYPE ANNOTATIONS


typeAnnotationDecoder : Decoder CanonicalTypeAnnotation
typeAnnotationDecoder =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "UnitType" ->
                        D.succeed CUnitType

                    "TypeVariable" ->
                        D.field "name" D.string |> D.map CGenericType

                    "TypeReference" ->
                        D.map3
                            (\mod name args ->
                                -- elm-format adds implicit module names for well-known types
                                -- (Maybe, List, Result, etc.), but elm-syntax records what's
                                -- written in source. Normalize by stripping module if it equals
                                -- the type name (implicit import).
                                let
                                    normalizedMod =
                                        if mod == [ name ] then
                                            []

                                        else
                                            mod
                                in
                                CTyped normalizedMod name args
                            )
                            (D.field "module"
                                (D.nullable D.string
                                    |> D.map
                                        (\m ->
                                            case m of
                                                Just s ->
                                                    String.split "." s

                                                Nothing ->
                                                    []
                                        )
                                )
                            )
                            (D.field "name" D.string)
                            (D.field "arguments" (D.list (D.lazy (\_ -> typeAnnotationDecoder))))

                    "TupleType" ->
                        D.field "terms" (D.list (D.lazy (\_ -> typeAnnotationDecoder)))
                            |> D.map CTupleType

                    "RecordType" ->
                        recordTypeDecoder

                    "FunctionType" ->
                        D.map2
                            (\argTypes returnType ->
                                -- elm-format gives argumentTypes as a list; build right-associative chain
                                List.foldr CFunctionType returnType argTypes
                            )
                            (D.field "argumentTypes" (D.list (D.lazy (\_ -> typeAnnotationDecoder))))
                            (D.field "returnType" (D.lazy (\_ -> typeAnnotationDecoder)))

                    other ->
                        D.fail ("Unknown type annotation tag: " ++ other)
            )


recordTypeDecoder : Decoder CanonicalTypeAnnotation
recordTypeDecoder =
    D.map3
        (\maybeVar fieldPairs fieldOrder ->
            let
                orderedFields =
                    List.filterMap
                        (\name ->
                            List.head (List.filter (\( k, _ ) -> k == name) fieldPairs)
                        )
                        fieldOrder
            in
            case maybeVar of
                Just var ->
                    CGenericRecordType var orderedFields

                Nothing ->
                    CRecordType orderedFields
        )
        (D.maybe (D.field "base" D.string))
        (D.field "fields" (D.keyValuePairs (D.lazy (\_ -> typeAnnotationDecoder))))
        (D.field "display" (D.field "fieldOrder" (D.list D.string)))
