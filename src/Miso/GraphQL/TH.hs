module Miso.GraphQL.TH where

import Data.Char (toUpper)
import Data.Foldable (for_, msum, toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Language.Haskell.TH hiding (Name, Type)
import Language.Haskell.TH qualified as TH hiding (Type)
import Language.Haskell.TH.Syntax
    ( Quasi (qAddDependentFile)
    , addModFinalizer
    , makeRelativeToProject
    )
import Miso.GraphQL.AST hiding (rootOperationType)
import Miso.GraphQL.Class (ToGraphQL (..))
import Miso.GraphQL.JSON (Operation (..))
import Miso.GraphQL.Lexer qualified as Lexer
import Miso.GraphQL.Parser qualified as Parser
import Miso.JSON (FromJSON, ToJSON)
import Miso.Prelude hiding (for)
import Miso.String (ToMisoString)
import Miso.String qualified as MisoString

documentFile :: FilePath -> DecsQ
documentFile f = do
    f <- makeRelativeToProject f
    qAddDependentFile f
    src <- runIO $ readFile f
    liftIO $ putStrLn src
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

maybeConcat :: (Semigroup a) => Maybe a -> Maybe a -> Maybe a
maybeConcat Nothing Nothing = Nothing
maybeConcat (Just a) Nothing = Just a
maybeConcat Nothing (Just b) = Just b
maybeConcat (Just a) (Just b) = Just (a <> b)

applyExtension :: TypeExtension -> TypeDefinition -> TypeDefinition
applyExtension
    (ExtensionScalarType (ScalarTypeExtension name' directives'))
    (DefinitionScalarType (ScalarTypeDefinition desc name directives))
        | name == name' =
            DefinitionScalarType
                $ ScalarTypeDefinition desc name (directives `maybeConcat` Just directives')
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
                    (implementsInterfaces `maybeConcat` implementsInterfaces')
                    (directives `maybeConcat` directives')
                    (fieldsDefinition `maybeConcat` fieldsDefinition')
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
                    (implementsInterfaces `maybeConcat` implementsInterfaces')
                    (directives `maybeConcat` directives')
                    (fieldsDefinition `maybeConcat` fieldsDefinition')
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
                    (directives `maybeConcat` directives')
                    (unionMemberTypes `maybeConcat` unionMemberTypes')
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
                    (directives `maybeConcat` directives')
                    (enumValuesDefinition `maybeConcat` enumValuesDefinition')
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
                    (directives `maybeConcat` directives')
                    (fieldsDefinition `maybeConcat` fieldsDefinition')
applyExtension _ t = t

document :: Document -> DecsQ
document (Document definitions) =
    mconcat
        . mconcat
        $ [ typeDefinitions
                & Map.toList
                & filter (\(name, _) -> not $ isRootOperationType name)
                <&> \(_, typeDefinition) ->
                    case typeDefinition of
                        DefinitionScalarType _ -> pure []
                        DefinitionObjectType typeDefinition ->
                            objectTypeDefinition typeDefinition
                        DefinitionInterfaceType _ -> pure []
                        DefinitionUnionType typeDefinition ->
                            unionTypeDefinition typeDefinition
                        DefinitionEnumType typeDefinition ->
                            enumTypeDefinition typeDefinition
                        DefinitionInputObjectType typeDefinition ->
                            inputObjectTypeDefinition typeDefinition
          , rootOperationTypes <&> \(operationType, NamedType name) -> do
                typeDefinition <- case Map.lookup name typeDefinitions of
                    Just (DefinitionObjectType typeDefinition) -> pure typeDefinition
                    Just _ -> fail $ "Expected object type for root operation " <> show operationType
                    Nothing -> fail $ "Cannot find type for root operation " <> show operationType
                rootOperationType typeDefinitions operationType typeDefinition
          ]
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
    rootOperationTypes :: [(OperationType, NamedType)]
    rootOperationTypes = flip mapMaybe [minBound .. maxBound] \ot ->
        (ot,)
            <$> msum
                [ listToMaybe
                    [ t
                    | RootOperationTypeDefinition o t <- toList rootOperations
                    , o == ot
                    ]
                , let defaultName = Name . toMisoString $ show ot
                   in NamedType defaultName <$ Map.lookup defaultName typeDefinitions
                ]
    isRootOperationType :: Name -> Bool
    isRootOperationType = (`elem` (namedTypeName . snd <$> rootOperationTypes))

putDoc' :: TH.Name -> String -> Q ()
putDoc' name = addModFinalizer . putDoc (DeclDoc name)

description :: TH.Name -> Description -> Q ()
description name =
    putDoc' name . fromMisoString . \case
        Description (SingleLineString s) -> s
        Description (BlockString s) -> s

mkName' :: (ToMisoString s) => s -> TH.Name
mkName' = mkName . fromMisoString . toMisoString

objectTypeDefinition :: ObjectTypeDefinition -> DecsQ
objectTypeDefinition (ObjectTypeDefinition desc name _ _ fields) = do
    for_ desc $ description name'
    case length fields' of
        0 -> pure []
        1 -> pure <$> newtypeD (pure mempty) name' mempty Nothing con derivs
        _ -> pure <$> dataD (pure mempty) name' mempty Nothing [con] derivs
  where
    name' = mkName' name
    fields' =
        fields & concatMap \(FieldsDefinition xs) ->
            xs & toList & filter \(FieldDefinition _ _ args _ _) ->
                isNothing args
    con :: ConQ
    con =
        recC name' $ fields' <&> \(FieldDefinition desc name _ t _) -> do
            let name' = mkName' name
            for_ desc $ description name'
            (name',defaultBang,) <$> type' t
    derivs :: [DerivClauseQ]
    derivs =
        [ derivClause (Just StockStrategy) [conT ''Eq, conT ''Generic]
        , derivClause
            (Just AnyclassStrategy)
            [conT ''ToJSON, conT ''FromJSON, conT ''ToGraphQL]
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
        members' <&> \nt@(NamedType memberName) -> do
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

rootOperationType
    :: Map Name TypeDefinition
    -> OperationType
    -> ObjectTypeDefinition
    -> DecsQ
rootOperationType typeDefinitions ot (ObjectTypeDefinition _ name _ _ fields) =
    mconcat <$> for fields' (rootOperation typeDefinitions ot name)
  where
    fields' = fields & concatMap \(FieldsDefinition xs) -> toList xs

rootOperation
    :: Map Name TypeDefinition
    -> OperationType
    -> Name
    -> FieldDefinition
    -> DecsQ
rootOperation _ Subscription _ _ = pure []
rootOperation typeDefinitions ot (Name name) (FieldDefinition desc (Name fieldName) args t _) = do
    for_ desc $ description name'
    sequence
        [ case length args' of
            1 -> newtypeD (pure mempty) name' mempty Nothing con derivs
            _ -> dataD (pure mempty) name' mempty Nothing [con] derivs
        , instanceD
            mempty
            (conT ''Operation `appT` conT name')
            [ tySynInstD $ tySynEqn Nothing (conT ''ReturnType `appT` conT name') (type' t)
            , funD 'toOperation
                . pure
                $ clause [conP name' [varP f | ((_, f), _) <- args']] toOperationBody []
            ]
            -- , sigD name' returnType
            -- , funD name' . pure $ clause (varP . snd . fst <$> args') body mempty
        ]
  where
    capitalise :: MisoString -> MisoString
    capitalise s
        | Just (x, xs) <- MisoString.uncons s = MisoString.cons (toUpper x) xs
        | otherwise = s
    name' = mkName' $ name <> capitalise fieldName
    con :: ConQ
    con =
        recC name' $ args' <&> \((_, n), t) -> (n,defaultBang,) <$> type' t
    derivs :: [DerivClauseQ]
    derivs =
        [ derivClause (Just StockStrategy) [conT ''Eq, conT ''Generic]
        , derivClause
            (Just AnyclassStrategy)
            [conT ''ToJSON, conT ''FromJSON, conT ''ToGraphQL]
        ]
    args' :: [((Name, TH.Name), Type)]
    args' =
        case args of
            Nothing -> []
            Just (ArgumentsDefinition args) ->
                toList args <&> \(InputValueDefinition _ n t _ _) -> ((n, mkName' n), t)
    otE =
        conE $ case ot of
            Query -> 'Query
            Mutation -> 'Mutation
    stringE' :: (ToMisoString s) => s -> ExpQ
    stringE' = stringE . fromMisoString . toMisoString
    argsE :: ExpQ
    argsE =
        case args' of
            [] -> conE 'Nothing
            _ ->
                varE 'nonEmpty
                    `appE` listE
                        [ [|Argument (Name $(stringE' name)) (toGraphQL $(varE thName))|]
                        | ((name, thName), _) <- args'
                        ]
    fields :: Type -> [FieldDefinition]
    fields t =
        Map.lookup (typeName t) typeDefinitions
            & concatMap \case
                ( DefinitionObjectType
                        (ObjectTypeDefinition _ _ _ _ (Just (FieldsDefinition fields)))
                    ) -> toList fields
                _ -> []
    selE :: Type -> ExpQ
    selE t =
        case fields t of
            [] -> conE 'Nothing
            fields ->
                varE 'nonEmpty
                    `appE` listE
                        [ [|
                            SelectionField
                                $ Field
                                    Nothing
                                    (Name $(stringE' name))
                                    Nothing
                                    Nothing
                                    $(selE t)
                            |]
                        | FieldDefinition _ name _ t _ <- fields
                        ]
    toOperationBody :: BodyQ
    toOperationBody =
        normalB
            [|
                OperationDefinition
                    Nothing
                    $(otE)
                    Nothing
                    Nothing
                    Nothing
                    . pure
                    . SelectionField
                    $ Field Nothing $(stringE' fieldName) $(argsE) Nothing $(selE t)
                |]

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

type ID = MisoString

namedType :: NamedType -> TypeQ
namedType (NamedType (Name "Int")) = conT ''Int
namedType (NamedType (Name "Float")) = conT ''Double
namedType (NamedType (Name "String")) = conT ''MisoString
namedType (NamedType (Name "Boolean")) = conT ''Bool
namedType (NamedType (Name "ID")) = conT ''ID
namedType (NamedType name) = conT $ mkName' name

listType :: ListType -> TypeQ
listType (ListType t) = listT `appT` type' t

type' :: Miso.GraphQL.AST.Type -> TypeQ
type' (TypeNamed t) = conT (mkName "Maybe") `appT` namedType t
type' (TypeList t) = conT (mkName "Maybe") `appT` listType t
type' (TypeNonNull (NonNullTypeNamed t)) = namedType t
type' (TypeNonNull (NonNullTypeList t)) = listType t
