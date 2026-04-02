module Canonical.Encode exposing (encodeFile)

import Canonical exposing (..)
import Json.Encode as E exposing (Value)


encodeFile : CanonicalFile -> Value
encodeFile file =
    E.object
        [ ( "moduleName", E.list E.string file.moduleName )
        , ( "declarations", E.list encodeDeclaration file.declarations )
        ]


encodeDeclaration : CanonicalDeclaration -> Value
encodeDeclaration decl =
    case decl of
        CanonicalFunction def ->
            tagged "function" (encodeFunctionDef def)

        CanonicalTypeAlias def ->
            tagged "typeAlias"
                (E.object
                    [ ( "name", E.string def.name )
                    , ( "generics", E.list E.string def.generics )
                    , ( "definition", encodeTypeAnnotation def.definition )
                    ]
                )

        CanonicalCustomType def ->
            tagged "customType"
                (E.object
                    [ ( "name", E.string def.name )
                    , ( "generics", E.list E.string def.generics )
                    , ( "constructors", E.list encodeConstructor def.constructors )
                    ]
                )

        CanonicalPort def ->
            tagged "port"
                (E.object
                    [ ( "name", E.string def.name )
                    , ( "typeAnnotation", encodeTypeAnnotation def.typeAnnotation )
                    ]
                )

        CanonicalDestructuring pattern expr ->
            tagged "destructuring"
                (E.object
                    [ ( "pattern", encodePattern pattern )
                    , ( "expression", encodeExpression expr )
                    ]
                )


encodeFunctionDef : CanonicalFunctionDef -> Value
encodeFunctionDef def =
    E.object
        [ ( "name", E.string def.name )
        , ( "typeAnnotation"
          , case def.typeAnnotation of
                Just ta ->
                    encodeTypeAnnotation ta

                Nothing ->
                    E.null
          )
        , ( "arguments", E.list encodePattern def.arguments )
        , ( "body", encodeExpression def.body )
        ]


encodeExpression : CanonicalExpression -> Value
encodeExpression expr =
    case expr of
        CUnit ->
            tagged "unit" E.null

        CApplication exprs ->
            tagged "application" (E.list encodeExpression exprs)

        CFunctionOrValue moduleName name ->
            tagged "functionOrValue"
                (E.object
                    [ ( "moduleName", E.list E.string moduleName )
                    , ( "name", E.string name )
                    ]
                )

        CIfBlock cond then_ else_ ->
            tagged "ifBlock"
                (E.object
                    [ ( "condition", encodeExpression cond )
                    , ( "then", encodeExpression then_ )
                    , ( "else", encodeExpression else_ )
                    ]
                )

        CPrefixOperator op ->
            tagged "prefixOperator" (E.string op)

        CInt n ->
            tagged "int" (E.int n)

        CFloat n ->
            tagged "float" (E.float n)

        CNegation inner ->
            tagged "negation" (encodeExpression inner)

        CStringLiteral s ->
            tagged "string" (E.string s)

        CCharLiteral c ->
            tagged "char" (E.string (String.fromChar c))

        CTuple exprs ->
            tagged "tuple" (E.list encodeExpression exprs)

        CList exprs ->
            tagged "list" (E.list encodeExpression exprs)

        CLet decls body ->
            tagged "let"
                (E.object
                    [ ( "declarations", E.list encodeLetDeclaration decls )
                    , ( "body", encodeExpression body )
                    ]
                )

        CCase subject branches ->
            tagged "case"
                (E.object
                    [ ( "subject", encodeExpression subject )
                    , ( "branches"
                      , E.list
                            (\( pat, body ) ->
                                E.object
                                    [ ( "pattern", encodePattern pat )
                                    , ( "body", encodeExpression body )
                                    ]
                            )
                            branches
                      )
                    ]
                )

        CLambda args body ->
            tagged "lambda"
                (E.object
                    [ ( "args", E.list encodePattern args )
                    , ( "body", encodeExpression body )
                    ]
                )

        CRecordExpr fields ->
            tagged "record"
                (E.list
                    (\( name, val ) ->
                        E.object
                            [ ( "name", E.string name )
                            , ( "value", encodeExpression val )
                            ]
                    )
                    fields
                )

        CRecordAccess inner field ->
            tagged "recordAccess"
                (E.object
                    [ ( "expression", encodeExpression inner )
                    , ( "field", E.string field )
                    ]
                )

        CRecordAccessFunction field ->
            tagged "recordAccessFunction" (E.string field)

        CRecordUpdate name fields ->
            tagged "recordUpdate"
                (E.object
                    [ ( "name", E.string name )
                    , ( "fields"
                      , E.list
                            (\( fname, val ) ->
                                E.object
                                    [ ( "name", E.string fname )
                                    , ( "value", encodeExpression val )
                                    ]
                            )
                            fields
                      )
                    ]
                )

        CGlsl code ->
            tagged "glsl" (E.string code)


encodeLetDeclaration : CanonicalLetDeclaration -> Value
encodeLetDeclaration decl =
    case decl of
        CLetFunction def ->
            tagged "function" (encodeFunctionDef def)

        CLetDestructuring pattern expr ->
            tagged "destructuring"
                (E.object
                    [ ( "pattern", encodePattern pattern )
                    , ( "expression", encodeExpression expr )
                    ]
                )


encodePattern : CanonicalPattern -> Value
encodePattern pattern =
    case pattern of
        CAllPattern ->
            tagged "all" E.null

        CUnitPattern ->
            tagged "unit" E.null

        CCharPattern c ->
            tagged "char" (E.string (String.fromChar c))

        CStringPattern s ->
            tagged "string" (E.string s)

        CIntPattern n ->
            tagged "int" (E.int n)

        CFloatPattern n ->
            tagged "float" (E.float n)

        CTuplePattern pats ->
            tagged "tuple" (E.list encodePattern pats)

        CRecordPattern fields ->
            tagged "record" (E.list E.string fields)

        CConsPattern head tail ->
            tagged "cons"
                (E.object
                    [ ( "head", encodePattern head )
                    , ( "tail", encodePattern tail )
                    ]
                )

        CListPattern pats ->
            tagged "list" (E.list encodePattern pats)

        CVarPattern name ->
            tagged "var" (E.string name)

        CNamedPattern moduleName name args ->
            tagged "named"
                (E.object
                    [ ( "moduleName", E.list E.string moduleName )
                    , ( "name", E.string name )
                    , ( "args", E.list encodePattern args )
                    ]
                )

        CAsPattern pat name ->
            tagged "as"
                (E.object
                    [ ( "pattern", encodePattern pat )
                    , ( "name", E.string name )
                    ]
                )


encodeTypeAnnotation : CanonicalTypeAnnotation -> Value
encodeTypeAnnotation ta =
    case ta of
        CGenericType name ->
            tagged "generic" (E.string name)

        CTyped moduleName name args ->
            tagged "typed"
                (E.object
                    [ ( "moduleName", E.list E.string moduleName )
                    , ( "name", E.string name )
                    , ( "args", E.list encodeTypeAnnotation args )
                    ]
                )

        CUnitType ->
            tagged "unit" E.null

        CTupleType types ->
            tagged "tuple" (E.list encodeTypeAnnotation types)

        CRecordType fields ->
            tagged "record" (E.list encodeRecordField fields)

        CGenericRecordType var fields ->
            tagged "genericRecord"
                (E.object
                    [ ( "var", E.string var )
                    , ( "fields", E.list encodeRecordField fields )
                    ]
                )

        CFunctionType from to ->
            tagged "function"
                (E.object
                    [ ( "from", encodeTypeAnnotation from )
                    , ( "to", encodeTypeAnnotation to )
                    ]
                )


encodeRecordField : ( String, CanonicalTypeAnnotation ) -> Value
encodeRecordField ( name, ta ) =
    E.object
        [ ( "name", E.string name )
        , ( "type", encodeTypeAnnotation ta )
        ]


encodeConstructor : CanonicalConstructor -> Value
encodeConstructor ctor =
    E.object
        [ ( "name", E.string ctor.name )
        , ( "arguments", E.list encodeTypeAnnotation ctor.arguments )
        ]


tagged : String -> Value -> Value
tagged tag value =
    E.object
        [ ( "tag", E.string tag )
        , ( "value", value )
        ]
