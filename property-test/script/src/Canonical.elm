module Canonical exposing
    ( CanonicalConstructor
    , CanonicalCustomTypeDef
    , CanonicalDeclaration(..)
    , CanonicalExpression(..)
    , CanonicalFile
    , CanonicalFunctionDef
    , CanonicalLetDeclaration(..)
    , CanonicalPattern(..)
    , CanonicalPortDef
    , CanonicalTypeAliasDef
    , CanonicalTypeAnnotation(..)
    )

{-| Canonical AST types used as a common target for comparing
elm-syntax and elm-format parse results.

All source locations are stripped. Parenthesized expressions/patterns
are unwrapped. Integer/Hex are merged. Comments are dropped.

-}


type alias CanonicalFile =
    { moduleName : List String
    , declarations : List CanonicalDeclaration
    }


type CanonicalDeclaration
    = CanonicalFunction CanonicalFunctionDef
    | CanonicalTypeAlias CanonicalTypeAliasDef
    | CanonicalCustomType CanonicalCustomTypeDef
    | CanonicalPort CanonicalPortDef
    | CanonicalDestructuring CanonicalPattern CanonicalExpression


type alias CanonicalFunctionDef =
    { name : String
    , typeAnnotation : Maybe CanonicalTypeAnnotation
    , arguments : List CanonicalPattern
    , body : CanonicalExpression
    }


type alias CanonicalTypeAliasDef =
    { name : String
    , generics : List String
    , definition : CanonicalTypeAnnotation
    }


type alias CanonicalCustomTypeDef =
    { name : String
    , generics : List String
    , constructors : List CanonicalConstructor
    }


type alias CanonicalConstructor =
    { name : String
    , arguments : List CanonicalTypeAnnotation
    }


type alias CanonicalPortDef =
    { name : String
    , typeAnnotation : CanonicalTypeAnnotation
    }


type CanonicalExpression
    = CUnit
    | CApplication (List CanonicalExpression)
    | CFunctionOrValue (List String) String
    | CIfBlock CanonicalExpression CanonicalExpression CanonicalExpression
    | CPrefixOperator String
    | CInt Int
    | CFloat Float
    | CNegation CanonicalExpression
    | CStringLiteral String
    | CCharLiteral Char
    | CTuple (List CanonicalExpression)
    | CList (List CanonicalExpression)
    | CLet (List CanonicalLetDeclaration) CanonicalExpression
    | CCase CanonicalExpression (List ( CanonicalPattern, CanonicalExpression ))
    | CLambda (List CanonicalPattern) CanonicalExpression
    | CRecordExpr (List ( String, CanonicalExpression ))
    | CRecordAccess CanonicalExpression String
    | CRecordAccessFunction String
    | CRecordUpdate String (List ( String, CanonicalExpression ))
    | CGlsl String


type CanonicalLetDeclaration
    = CLetFunction CanonicalFunctionDef
    | CLetDestructuring CanonicalPattern CanonicalExpression


type CanonicalPattern
    = CAllPattern
    | CUnitPattern
    | CCharPattern Char
    | CStringPattern String
    | CIntPattern Int
    | CFloatPattern Float
    | CTuplePattern (List CanonicalPattern)
    | CRecordPattern (List String)
    | CConsPattern CanonicalPattern CanonicalPattern
    | CListPattern (List CanonicalPattern)
    | CVarPattern String
    | CNamedPattern (List String) String (List CanonicalPattern)
    | CAsPattern CanonicalPattern String


type CanonicalTypeAnnotation
    = CGenericType String
    | CTyped (List String) String (List CanonicalTypeAnnotation)
    | CUnitType
    | CTupleType (List CanonicalTypeAnnotation)
    | CRecordType (List ( String, CanonicalTypeAnnotation ))
    | CGenericRecordType String (List ( String, CanonicalTypeAnnotation ))
    | CFunctionType CanonicalTypeAnnotation CanonicalTypeAnnotation
