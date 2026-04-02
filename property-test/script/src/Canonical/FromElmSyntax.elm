module Canonical.FromElmSyntax exposing (fromFile)

{-| Convert elm-syntax AST directly to canonical form.
No JSON round-trip needed since we can import elm-syntax types directly.
-}

import Canonical exposing (..)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File as File
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Signature as Signature
import Elm.Syntax.TypeAnnotation as TypeAnnotation


fromFile : File.File -> CanonicalFile
fromFile file =
    { moduleName = extractModuleName file.moduleDefinition
    , declarations = List.filterMap (fromDeclaration << Node.value) file.declarations
    }


extractModuleName : Node Module.Module -> List String
extractModuleName (Node _ mod) =
    case mod of
        Module.NormalModule data ->
            Node.value data.moduleName

        Module.PortModule data ->
            Node.value data.moduleName

        Module.EffectModule data ->
            Node.value data.moduleName


fromDeclaration : Declaration.Declaration -> Maybe CanonicalDeclaration
fromDeclaration decl =
    case decl of
        Declaration.FunctionDeclaration fn ->
            Just (CanonicalFunction (fromFunction fn))

        Declaration.AliasDeclaration alias_ ->
            Just
                (CanonicalTypeAlias
                    { name = Node.value alias_.name
                    , generics = List.map Node.value alias_.generics
                    , definition = fromTypeAnnotation (Node.value alias_.typeAnnotation)
                    }
                )

        Declaration.CustomTypeDeclaration type_ ->
            Just
                (CanonicalCustomType
                    { name = Node.value type_.name
                    , generics = List.map Node.value type_.generics
                    , constructors =
                        List.map
                            (\(Node _ ctor) ->
                                { name = Node.value ctor.name
                                , arguments = List.map (fromTypeAnnotation << Node.value) ctor.arguments
                                }
                            )
                            type_.constructors
                    }
                )

        Declaration.PortDeclaration sig ->
            Just
                (CanonicalPort
                    { name = Node.value sig.name
                    , typeAnnotation = fromTypeAnnotation (Node.value sig.typeAnnotation)
                    }
                )

        Declaration.Destructuring pat expr ->
            Just
                (CanonicalDestructuring
                    (fromPattern (Node.value pat))
                    (fromExpression (Node.value expr))
                )

        Declaration.InfixDeclaration _ ->
            Nothing


fromFunction : Expression.Function -> CanonicalFunctionDef
fromFunction fn =
    let
        (Node _ impl) =
            fn.declaration
    in
    { name = Node.value impl.name
    , typeAnnotation =
        fn.signature
            |> Maybe.map
                (\(Node _ sig) ->
                    fromTypeAnnotation (Node.value sig.typeAnnotation)
                )
    , arguments = List.map (fromPattern << Node.value) impl.arguments
    , body = fromExpression (Node.value impl.expression)
    }


fromExpression : Expression.Expression -> CanonicalExpression
fromExpression expr =
    case expr of
        Expression.UnitExpr ->
            CUnit

        Expression.Application nodes ->
            CApplication (List.map (fromExpression << Node.value) nodes)

        Expression.OperatorApplication op _ (Node _ left) (Node _ right) ->
            CApplication
                [ CFunctionOrValue [] op
                , fromExpression left
                , fromExpression right
                ]

        Expression.FunctionOrValue moduleName name ->
            CFunctionOrValue moduleName name

        Expression.IfBlock (Node _ cond) (Node _ then_) (Node _ else_) ->
            CIfBlock (fromExpression cond) (fromExpression then_) (fromExpression else_)

        Expression.PrefixOperator op ->
            CPrefixOperator op

        Expression.Operator op ->
            CPrefixOperator op

        Expression.Integer n ->
            CInt n

        Expression.Hex n ->
            CInt n

        Expression.Floatable f ->
            CFloat f

        Expression.Negation (Node _ inner) ->
            CNegation (fromExpression inner)

        Expression.Literal s ->
            CStringLiteral s

        Expression.CharLiteral c ->
            CCharLiteral c

        Expression.TupledExpression nodes ->
            CTuple (List.map (fromExpression << Node.value) nodes)

        Expression.ParenthesizedExpression (Node _ inner) ->
            fromExpression inner

        Expression.LetExpression letBlock ->
            CLet
                (List.map (fromLetDeclaration << Node.value) letBlock.declarations)
                (fromExpression (Node.value letBlock.expression))

        Expression.CaseExpression caseBlock ->
            CCase
                (fromExpression (Node.value caseBlock.expression))
                (List.map
                    (\( Node _ pat, Node _ body ) ->
                        ( fromPattern pat, fromExpression body )
                    )
                    caseBlock.cases
                )

        Expression.LambdaExpression lambda ->
            CLambda
                (List.map (fromPattern << Node.value) lambda.args)
                (fromExpression (Node.value lambda.expression))

        Expression.RecordExpr fields ->
            CRecordExpr
                (List.map
                    (\(Node _ ( Node _ name, Node _ val )) ->
                        ( name, fromExpression val )
                    )
                    fields
                )

        Expression.ListExpr nodes ->
            CList (List.map (fromExpression << Node.value) nodes)

        Expression.RecordAccess (Node _ inner) (Node _ field) ->
            CRecordAccess (fromExpression inner) field

        Expression.RecordAccessFunction field ->
            CRecordAccessFunction (String.dropLeft 1 field)

        Expression.RecordUpdateExpression (Node _ name) fields ->
            CRecordUpdate name
                (List.map
                    (\(Node _ ( Node _ fname, Node _ val )) ->
                        ( fname, fromExpression val )
                    )
                    fields
                )

        Expression.GLSLExpression code ->
            CGlsl code


fromLetDeclaration : Expression.LetDeclaration -> CanonicalLetDeclaration
fromLetDeclaration decl =
    case decl of
        Expression.LetFunction fn ->
            CLetFunction (fromFunction fn)

        Expression.LetDestructuring (Node _ pat) (Node _ expr) ->
            CLetDestructuring (fromPattern pat) (fromExpression expr)


fromPattern : Pattern.Pattern -> CanonicalPattern
fromPattern pat =
    case pat of
        Pattern.AllPattern ->
            CAllPattern

        Pattern.UnitPattern ->
            CUnitPattern

        Pattern.CharPattern c ->
            CCharPattern c

        Pattern.StringPattern s ->
            CStringPattern s

        Pattern.IntPattern n ->
            CIntPattern n

        Pattern.HexPattern n ->
            CIntPattern n

        Pattern.FloatPattern f ->
            CFloatPattern f

        Pattern.TuplePattern nodes ->
            CTuplePattern (List.map (fromPattern << Node.value) nodes)

        Pattern.RecordPattern nodes ->
            CRecordPattern (List.sort (List.map Node.value nodes))

        Pattern.UnConsPattern (Node _ head) (Node _ tail) ->
            CConsPattern (fromPattern head) (fromPattern tail)

        Pattern.ListPattern nodes ->
            CListPattern (List.map (fromPattern << Node.value) nodes)

        Pattern.VarPattern name ->
            CVarPattern name

        Pattern.NamedPattern ref args ->
            CNamedPattern ref.moduleName ref.name (List.map (fromPattern << Node.value) args)

        Pattern.AsPattern (Node _ inner) (Node _ name) ->
            CAsPattern (fromPattern inner) name

        Pattern.ParenthesizedPattern (Node _ inner) ->
            fromPattern inner


fromTypeAnnotation : TypeAnnotation.TypeAnnotation -> CanonicalTypeAnnotation
fromTypeAnnotation ta =
    case ta of
        TypeAnnotation.GenericType name ->
            CGenericType name

        TypeAnnotation.Typed (Node _ ( moduleName, name )) args ->
            CTyped moduleName name (List.map (fromTypeAnnotation << Node.value) args)

        TypeAnnotation.Unit ->
            CUnitType

        TypeAnnotation.Tupled types ->
            CTupleType (List.map (fromTypeAnnotation << Node.value) types)

        TypeAnnotation.Record fields ->
            CRecordType
                (List.map
                    (\(Node _ ( Node _ name, Node _ ta_ )) ->
                        ( name, fromTypeAnnotation ta_ )
                    )
                    fields
                )

        TypeAnnotation.GenericRecord (Node _ var) (Node _ fields) ->
            CGenericRecordType var
                (List.map
                    (\(Node _ ( Node _ name, Node _ ta_ )) ->
                        ( name, fromTypeAnnotation ta_ )
                    )
                    fields
                )

        TypeAnnotation.FunctionTypeAnnotation (Node _ from) (Node _ to) ->
            CFunctionType (fromTypeAnnotation from) (fromTypeAnnotation to)
