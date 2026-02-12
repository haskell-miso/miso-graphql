{-# LANGUAGE AllowAmbiguousTypes #-}

module Miso.GraphQL.TH
    ( IsRootOperationType (..)
    , NamedType (..)
    , ImplementsInterface
    , ID
    , documentFile
    )
where

import Data.Coerce (coerce)
import Data.Foldable (for_, msum, toList)
import Data.Foldable1 (foldrM1)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
    ( fromMaybe
    , isJust
    , listToMaybe
    , mapMaybe
    , maybeToList
    )
import Data.Row
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Language.Haskell.TH hiding (Name, Type)
import Language.Haskell.TH qualified as TH hiding (Type)
import Language.Haskell.TH.Syntax
    ( Quasi (qAddDependentFile)
    , addModFinalizer
    , makeRelativeToProject
    )
import Miso.GraphQL.AST hiding (NamedType, rootOperationType)
import Miso.GraphQL.AST qualified as AST
import Miso.GraphQL.Class (FromGraphQL, ToGraphQL (..))
import Miso.GraphQL.Lexer qualified as Lexer
import Miso.GraphQL.Parser qualified as Parser
import Miso.JSON (FromJSON, ToJSON)
import Miso.Prelude hiding (for)
import Miso.String (ToMisoString)

class IsRootOperationType t where
    operationType :: OperationType

class NamedType t where
    type TypeName t :: Symbol

class (NamedType t) => ImplementsInterface i t

documentFile :: FilePath -> DecsQ
documentFile f = do
    f <- makeRelativeToProject f
    qAddDependentFile f
    src <- runIO $ readFile f
    let src' = toMisoString src
    doc <-
        either (fail . show) pure $ Parser.parse' Lexer.tokens Parser.document src'
    document doc

typeDefinitionName :: TypeDefinition -> Name
typeDefinitionName (DefinitionScalarType (ScalarTypeDefinition _ name _)) = name
typeDefinitionName (DefinitionObjectType (ObjectTypeDefinition _ name _ _ _)) = name
typeDefinitionName (DefinitionInterfaceType (InterfaceTypeDefinition _ name _ _ _)) = name
typeDefinitionName (DefinitionUnionType (UnionTypeDefinition _ name _ _)) = name
typeDefinitionName (DefinitionEnumType (EnumTypeDefinition _ name _ _)) = name
typeDefinitionName (DefinitionInputObjectType (InputObjectTypeDefinition _ name _ _)) = name

typeExtensionName :: TypeExtension -> Name
typeExtensionName (ExtensionScalarType (ScalarTypeExtension name _)) = name
typeExtensionName (ExtensionObjectType (ObjectTypeExtension name _ _ _)) = name
typeExtensionName (ExtensionInterfaceType (InterfaceTypeExtension name _ _ _)) = name
typeExtensionName (ExtensionUnionType (UnionTypeExtension name _ _)) = name
typeExtensionName (ExtensionEnumType (EnumTypeExtension name _ _)) = name
typeExtensionName (ExtensionInputObjectType (InputObjectTypeExtension name _ _)) = name

applyExtension :: TypeExtension -> TypeDefinition -> TypeDefinition
applyExtension
    (ExtensionScalarType (ScalarTypeExtension name' directives'))
    (DefinitionScalarType (ScalarTypeDefinition desc name directives))
        | name == name' =
            DefinitionScalarType
                $ ScalarTypeDefinition desc name (directives <> Just directives')
applyExtension
    ( ExtensionObjectType
            (ObjectTypeExtension name' implementsInterfaces' directives' fieldsDefinition')
        )
    ( DefinitionObjectType
            (ObjectTypeDefinition desc name implementsInterfaces directives fieldsDefinition)
        )
        | name == name' =
            DefinitionObjectType
                $ ObjectTypeDefinition
                    desc
                    name
                    (implementsInterfaces <> implementsInterfaces')
                    (directives <> directives')
                    (fieldsDefinition <> fieldsDefinition')
applyExtension
    ( ExtensionInterfaceType
            (InterfaceTypeExtension name' implementsInterfaces' directives' fieldsDefinition')
        )
    ( DefinitionInterfaceType
            ( InterfaceTypeDefinition
                    desc
                    name
                    implementsInterfaces
                    directives
                    fieldsDefinition
                )
        )
        | name == name' =
            DefinitionInterfaceType
                $ InterfaceTypeDefinition
                    desc
                    name
                    (implementsInterfaces <> implementsInterfaces')
                    (directives <> directives')
                    (fieldsDefinition <> fieldsDefinition')
applyExtension
    ( ExtensionUnionType
            (UnionTypeExtension name' directives' unionMemberTypes')
        )
    ( DefinitionUnionType
            (UnionTypeDefinition desc name directives unionMemberTypes)
        )
        | name == name' =
            DefinitionUnionType
                $ UnionTypeDefinition
                    desc
                    name
                    (directives <> directives')
                    (unionMemberTypes <> unionMemberTypes')
applyExtension
    ( ExtensionEnumType
            (EnumTypeExtension name' directives' enumValuesDefinition')
        )
    ( DefinitionEnumType
            (EnumTypeDefinition desc name directives enumValuesDefinition)
        )
        | name == name' =
            DefinitionEnumType
                $ EnumTypeDefinition
                    desc
                    name
                    (directives <> directives')
                    (enumValuesDefinition <> enumValuesDefinition')
applyExtension
    ( ExtensionInputObjectType
            (InputObjectTypeExtension name' directives' fieldsDefinition')
        )
    ( DefinitionInputObjectType
            (InputObjectTypeDefinition desc name directives fieldsDefinition)
        )
        | name == name' =
            DefinitionInputObjectType
                $ InputObjectTypeDefinition
                    desc
                    name
                    (directives <> directives')
                    (fieldsDefinition <> fieldsDefinition')
applyExtension _ t = t

document :: Document -> DecsQ
document (Document definitions) =
    mconcat
        $ typeDefinitions
        & Map.toList
        <&> \(name, typeDefinition) ->
            case typeDefinition of
                DefinitionScalarType _ -> pure []
                DefinitionObjectType typeDefinition ->
                    objectTypeDefinition typeDefinition (rootOperationType name)
                DefinitionInterfaceType typeDefinition ->
                    interfaceTypeDefinition typeDefinition
                DefinitionUnionType typeDefinition ->
                    unionTypeDefinition typeDefinition
                DefinitionEnumType typeDefinition ->
                    enumTypeDefinition typeDefinition
                DefinitionInputObjectType typeDefinition ->
                    inputObjectTypeDefinition typeDefinition
  where
    typeDefinitions :: Map Name TypeDefinition
    typeDefinitions =
        Map.fromList
            [ (name, applyExtensions name t)
            | DefinitionTypeSystem (DefinitionType t) <- toList definitions
            , let name = typeDefinitionName t
            ]
    typeExtensions :: Map Name [TypeExtension]
    typeExtensions =
        Map.fromListWith
            (<>)
            [ (typeExtensionName t, pure t)
            | ExtensionTypeSystem (ExtensionType t) <- toList definitions
            ]
    applyExtensions :: Name -> TypeDefinition -> TypeDefinition
    applyExtensions name t = foldr applyExtension t . fromMaybe [] $ Map.lookup name typeExtensions
    rootOperations :: [RootOperationTypeDefinition]
    rootOperations =
        mconcat
            [ toList tds
            | DefinitionTypeSystem (DefinitionSchema (SchemaDefinition _ _ tds)) <-
                toList definitions
            ]
    rootOperationTypes :: [(OperationType, AST.NamedType)]
    rootOperationTypes = flip mapMaybe [minBound .. maxBound] \ot ->
        (ot,)
            <$> msum
                [ listToMaybe
                    [ t
                    | RootOperationTypeDefinition o t <- toList rootOperations
                    , o == ot
                    ]
                , let defaultName = Name . toMisoString $ show ot
                   in AST.NamedType defaultName <$ Map.lookup defaultName typeDefinitions
                ]
    rootOperationType :: Name -> Maybe OperationType
    rootOperationType name =
        fst
            <$> List.find (\(_, name') -> namedTypeName name' == name) rootOperationTypes

putDoc' :: TH.Name -> String -> Q ()
putDoc' name = addModFinalizer . putDoc (DeclDoc name)

description :: TH.Name -> Description -> Q ()
description name =
    putDoc' name . fromMisoString . \case
        Description (SingleLineString s) -> s
        Description (BlockString s) -> s

mkName' :: (ToMisoString s) => s -> TH.Name
mkName' = mkName . fromMisoString . toMisoString

objectTypeDefinition :: ObjectTypeDefinition -> Maybe OperationType -> DecsQ
objectTypeDefinition (ObjectTypeDefinition desc name interfaces _ fields) ot = do
    for_ desc $ description name'
    sequence $ case length fields' of
        0 -> []
        1 ->
            newtypeD (pure mempty) name' mempty Nothing con derivs
                : namedType
                : rootOperation <> implementsInterfaces
        _ ->
            dataD (pure mempty) name' mempty Nothing [con] derivs
                : namedType
                : rootOperation <> implementsInterfaces
  where
    name' = mkName' name
    fields' = fields & concatMap \(FieldsDefinition xs) -> toList xs
    con :: ConQ
    con =
        recC name' $ fields' <&> \(FieldDefinition desc name args t _) -> do
            let name' = mkName' name
            for_ desc $ description name'
            (name',defaultBang,) <$> typeWithArgs args t
    hasArgs = flip any fields' \(FieldDefinition _ _ args _ _) -> isJust args
    derivs :: [DerivClauseQ]
    derivs =
        mconcat
            [ pure $ derivClause (Just StockStrategy) [conT ''Generic]
            , [derivClause (Just StockStrategy) [conT ''Eq] | not hasArgs]
            , [ derivClause
                    (Just AnyclassStrategy)
                    [conT ''FromJSON, conT ''ToJSON, conT ''FromGraphQL, conT ''ToGraphQL]
              | not hasArgs
              ]
            ]
    rootOperation :: [DecQ]
    rootOperation =
        maybeToList ot <&> \ot ->
            let
                otE = conE case ot of
                    Query -> 'Query
                    Mutation -> 'Mutation
                    Subscription -> 'Subscription
             in
                instanceD
                    mempty
                    (conT ''IsRootOperationType `appT` conT name')
                    [funD 'operationType . pure $ clause [] (normalB otE) []]
    namedType :: DecQ
    namedType =
        instanceD
            mempty
            (conT ''NamedType `appT` conT name')
            [ tySynInstD
                $ tySynEqn
                    Nothing
                    (conT ''TypeName `appT` conT name')
                    (litT . strTyLit . fromMisoString . coerce $ name)
            ]
    implementsInterfaces :: [DecQ]
    implementsInterfaces =
        maybe [] (toList @NonEmpty . coerce) interfaces <&> \(AST.NamedType interfaceName) ->
            instanceD
                mempty
                (conT ''ImplementsInterface `appT` conT (mkName' interfaceName) `appT` conT name')
                []

interfaceTypeDefinition :: InterfaceTypeDefinition -> DecsQ
interfaceTypeDefinition (InterfaceTypeDefinition desc name _ _ fields) = do
    for_ desc $ description name'
    sequence $ case length fields' of
        0 -> []
        1 -> pure $ newtypeD (pure mempty) name' mempty Nothing con derivs
        _ -> pure $ dataD (pure mempty) name' mempty Nothing [con] derivs
  where
    name' = mkName' name
    fields' = fields & concatMap \(FieldsDefinition xs) -> toList xs
    con :: ConQ
    con =
        recC name' $ fields' <&> \(FieldDefinition desc name args t _) -> do
            let name' = mkName' name
            for_ desc $ description name'
            (name',defaultBang,) <$> typeWithArgs args t
    hasArgs = flip any fields' \(FieldDefinition _ _ args _ _) -> isJust args
    derivs :: [DerivClauseQ]
    derivs =
        mconcat
            [ pure $ derivClause (Just StockStrategy) [conT ''Generic]
            , [derivClause (Just StockStrategy) [conT ''Eq] | not hasArgs]
            , [ derivClause
                    (Just AnyclassStrategy)
                    [conT ''FromJSON, conT ''ToJSON, conT ''FromGraphQL, conT ''ToGraphQL]
              | not hasArgs
              ]
            ]

unionTypeDefinition :: UnionTypeDefinition -> DecsQ
unionTypeDefinition (UnionTypeDefinition desc name _ members) = do
    let name' = mkName' name
    for_ desc $ description name'
    pure
        <$> case members' of
            [member] -> tySynD name' [] $ namedType member
            _ -> dataD mempty name' mempty Nothing cons derivs
  where
    members' = members & concatMap \(UnionMemberTypes xs) -> toList xs
    cons :: [ConQ]
    cons =
        members' <&> \nt@(AST.NamedType memberName) -> do
            let memberName' = mkName' $ name <> memberName
            normalC memberName' . pure $ (defaultBang,) <$> namedType nt
    derivs :: [DerivClauseQ]
    derivs =
        [ derivClause (Just StockStrategy) [conT ''Eq, conT ''Generic]
        , derivClause (Just AnyclassStrategy) [conT ''ToJSON, conT ''ToGraphQL]
        ]

enumTypeDefinition :: EnumTypeDefinition -> DecsQ
enumTypeDefinition (EnumTypeDefinition desc name _ values) = do
    let name' = mkName' name
    for_ desc $ description name'
    pure <$> dataD mempty name' mempty Nothing cons derivs
  where
    values' = values & concatMap \(EnumValuesDefinition xs) -> toList xs
    cons :: [ConQ]
    cons =
        values' <&> \(EnumValueDefinition desc (EnumValue name) _) -> do
            let name' = mkName' name
            for_ desc $ description name'
            normalC name' []
    derivs :: [DerivClauseQ]
    derivs =
        [ derivClause
            (Just StockStrategy)
            [conT ''Generic, conT ''Eq, conT ''Bounded, conT ''Enum]
        , derivClause
            (Just AnyclassStrategy)
            [conT ''ToJSON, conT ''FromJSON, conT ''ToGraphQL]
        ]

inputObjectTypeDefinition :: InputObjectTypeDefinition -> DecsQ
inputObjectTypeDefinition (InputObjectTypeDefinition desc name _ fields) = do
    for_ desc $ description name'
    case length fields' of
        0 -> pure []
        1 -> pure <$> newtypeD (pure mempty) name' mempty Nothing con mempty
        _ -> pure <$> dataD (pure mempty) name' mempty Nothing [con] mempty
  where
    name' = mkName' name
    fields' = fields & concatMap \(InputFieldsDefinition xs) -> toList xs
    con :: ConQ
    con =
        recC name' $ fields' <&> \(InputValueDefinition desc name t _ _) -> do
            let name' = mkName' name
            for_ desc $ description name'
            (name',defaultBang,) <$> type' t

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

type ID = MisoString

namedType :: AST.NamedType -> TypeQ
namedType (AST.NamedType (Name "Int")) = conT ''Int
namedType (AST.NamedType (Name "Float")) = conT ''Double
namedType (AST.NamedType (Name "String")) = conT ''MisoString
namedType (AST.NamedType (Name "Boolean")) = conT ''Bool
namedType (AST.NamedType (Name "ID")) = conT ''ID
namedType (AST.NamedType name) = conT $ mkName' name

listType :: ListType -> TypeQ
listType (ListType t) = listT `appT` type' t

type' :: Type -> TypeQ
type' (TypeNamed t) = conT (mkName "Maybe") `appT` namedType t
type' (TypeList t) = conT (mkName "Maybe") `appT` listType t
type' (TypeNonNull (NonNullTypeNamed t)) = namedType t
type' (TypeNonNull (NonNullTypeList t)) = listType t

typeWithArgs :: Maybe ArgumentsDefinition -> Type -> TypeQ
typeWithArgs Nothing t = type' t
typeWithArgs (Just (ArgumentsDefinition args)) t = [t|Rec $args' -> $(type' t)|]
  where
    args' :: TypeQ
    args' =
        foldrM1 (\a b -> [t|$(pure a) .+ $(pure b)|])
            =<< mapM arg' args
    arg' :: InputValueDefinition -> TypeQ
    arg' (InputValueDefinition _ n t _ _) = [t|$(litT . strTyLit . fromMisoString . toMisoString $ n) .== $(type' t)|]
