{-# OPTIONS_GHC -Wno-orphans #-}

module Miso.GraphQL.Printer where

import Data.Bool (bool)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Miso.GraphQL.AST
import Miso.Prelude hiding (enclosed, lex, unwords)
import Miso.String (ToMisoString, intercalate, unwords)

withDesc :: Maybe Description -> MisoString -> MisoString
withDesc Nothing = id
withDesc (Just desc) = ((toMisoString desc <> "\n") <>)

enclosed :: MisoString -> MisoString -> MisoString -> MisoString
enclosed a b c = a <> c <> b

unwords' :: (Foldable f, ToMisoString a) => f a -> MisoString
unwords' xs = unwords $ toMisoString <$> toList xs

-- | https://spec.graphql.org/draft/#Document
instance ToMisoString Document where
    toMisoString (Document definitions) = unwords' definitions

-- | https://spec.graphql.org/draft/#Definition
instance ToMisoString Definition where
    toMisoString (DefinitionExecutable s) = toMisoString s
    toMisoString (DefinitionTypeSystem s) = toMisoString s
    toMisoString (ExtensionTypeSystem s) = toMisoString s

-- https://spec.graphql.org/draft/#ExecutableDefinition
instance ToMisoString ExecutableDefinition where
    toMisoString (DefinitionOperation s) = toMisoString s
    toMisoString (DefinitionFragment s) = toMisoString s

-- | A GraphQL 'OperationDefinition'
-- https://spec.graphql.org/draft/#OperationDefinition
instance ToMisoString OperationDefinition where
    toMisoString (AnonymousQuery selectionSet) = toMisoString selectionSet
    toMisoString (OperationDefinition desc ot name vars dirs selectionSet) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just $ toMisoString ot
              , toMisoString <$> name
              , toMisoString <$> vars
              , toMisoString <$> dirs
              , Just $ toMisoString selectionSet
              ]

-- | A GraphQL 'Operation' type
-- https://spec.graphql.org/draft/#OperationType
instance ToMisoString OperationType where
    toMisoString Query = "query"
    toMisoString Mutation = "mutation"
    toMisoString Subscription = "subscription"

-- | A GraphQL 'SelectionSet'
-- https://spec.graphql.org/draft/#SelectionSet
instance ToMisoString (NonEmpty Selection) where
    toMisoString = enclosed "{" "}" . unwords'

-- | A GraphQL 'Selection' type
-- https://spec.graphql.org/draft/#Selection
instance ToMisoString Selection where
    toMisoString (SelectionField field) = toMisoString field
    toMisoString (SelectionFragmentSpread fragmentSpread) = toMisoString fragmentSpread
    toMisoString (SelectionInlineFragment inlineFragment) = toMisoString inlineFragment

-- | A GraphQL 'Field' type
-- https://spec.graphql.org/draft/#Field
instance ToMisoString Field where
    toMisoString (Field alias name args dirs selectionSet) =
        mconcat
            . catMaybes
            $ [ toMisoString <$> alias
              , Just $ toMisoString name
              , toMisoString <$> args
              , toMisoString <$> dirs
              , Just $ toMisoString selectionSet
              ]

-- | A GraphQL 'Alias'
-- https://spec.graphql.org/draft/#Alias
instance ToMisoString Alias where
    toMisoString (Alias name) = toMisoString name <> ":"

-- | GraphQL 'Arguments'
-- https://spec.graphql.org/draft/#Arguments
instance ToMisoString Arguments where
    toMisoString = enclosed "(" ")" . unwords'

-- | A GraphQL 'Argument'
-- https://spec.graphql.org/draft/#Arguments
instance ToMisoString Argument where
    toMisoString (Argument name value) = toMisoString name <> ":" <> toMisoString value

-- | GraphQL 'FragmentSpread' type
-- https://spec.graphql.org/draft/#FragmentSpread
instance ToMisoString FragmentSpread where
    toMisoString (FragmentSpread fragmentName directives) =
        unwords ["...", toMisoString fragmentName, maybe "" toMisoString directives]

-- | GraphQL 'InlineFragment' type
-- https://spec.graphql.org/draft/#InlineFragment
instance ToMisoString InlineFragment where
    toMisoString (InlineFragment typeCondition directives selectionSet) =
        unwords
            . catMaybes
            $ [ Just "..."
              , toMisoString <$> typeCondition
              , toMisoString <$> directives
              , Just $ toMisoString selectionSet
              ]

-- | A GraphQL 'FragmentDefinition'
-- https://spec.graphql.org/draft/#FragmentDefinition
instance ToMisoString FragmentDefinition where
    toMisoString (FragmentDefinition desc fragmentName typeCondition directives selectionSet) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just "fragment"
              , Just $ toMisoString fragmentName
              , Just $ toMisoString typeCondition
              , toMisoString <$> directives
              , Just $ toMisoString selectionSet
              ]

-- | A GraphQL 'FragmentName'
-- https://spec.graphql.org/draft/#FragmentName
instance ToMisoString FragmentName where
    toMisoString (FragmentName name) = toMisoString name

-- | A GraphQL 'TypeCondition'
-- https://spec.graphql.org/draft/#TypeCondition
instance ToMisoString TypeCondition where
    toMisoString (TypeCondition namedType) = "on " <> toMisoString namedType

-- | A GraphQL 'Value'
-- https://spec.graphql.org/draft/#Value
instance ToMisoString Value where
    toMisoString (ValueVariable variable) = toMisoString variable
    toMisoString (ValueInt i) = toMisoString . show $ i
    toMisoString (ValueFloat f) = toMisoString . show $ f
    toMisoString (ValueString s) = toMisoString s
    toMisoString (ValueBoolean True) = "true"
    toMisoString (ValueBoolean False) = "false"
    toMisoString ValueNull = "null"
    toMisoString (ValueEnum enumValue) = toMisoString enumValue
    toMisoString (ValueList listValue) = toMisoString listValue
    toMisoString (ValueObject objectValue) = toMisoString objectValue

-- | https://spec.graphql.org/draft/#StringValue
instance ToMisoString StringValue where
    toMisoString (BlockString s) = "\"\"\"" <> s <> "\"\"\""
    toMisoString (SingleLineString s) = "\"" <> s <> "\""

-- | https://spec.graphql.org/draft/#ListValue
instance ToMisoString [Value] where
    toMisoString = enclosed "[" "]" . unwords'

-- | https://spec.graphql.org/draft/#ObjectValue
instance ToMisoString [ObjectField] where
    toMisoString = enclosed "{" "}" . unwords'

-- | A GraphQL 'EnumValue'
-- https://spec.graphql.org/draft/#EnumValue
instance ToMisoString EnumValue where
    toMisoString (EnumValue name) = toMisoString name

-- | A GraphQL 'ObjectField'
-- https://spec.graphql.org/draft/#ObjectField
instance ToMisoString ObjectField where
    toMisoString (ObjectField name value) =
        toMisoString name <> ":" <> toMisoString value

-- | GraphQL 'VariablesDefinition'
-- https://spec.graphql.org/draft/#VariablesDefinition
instance ToMisoString VariablesDefinition where
    toMisoString = enclosed "(" ")" . unwords'

-- | A GraphQL 'VariableDefinition'
-- https://spec.graphql.org/draft/#VariableDefinition
instance ToMisoString VariableDefinition where
    toMisoString (VariableDefinition desc variable type' defaultValue directives) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just $ toMisoString variable
              , Just ":"
              , Just $ toMisoString type'
              , toMisoString <$> defaultValue
              , toMisoString <$> directives
              ]

-- | A GraphQL 'Variable'
-- https://spec.graphql.org/draft/#Variable
instance ToMisoString Variable where
    toMisoString (Variable name) = "$" <> toMisoString name

-- | A GraphQL 'DefaultValue'
-- https://spec.graphql.org/draft/#DefaultValue
instance ToMisoString DefaultValue where
    toMisoString (DefaultValue value) = "=" <> toMisoString value

-- | A GraphQL 'Type'
-- https://spec.graphql.org/draft/#Type
instance ToMisoString Type where
    toMisoString (TypeNonNull s) = toMisoString s
    toMisoString (TypeList listType) = toMisoString listType
    toMisoString (TypeNamed namedType) = toMisoString namedType

-- | A GraphQL 'NamedType'
-- https://spec.graphql.org/draft/#NamedType
instance ToMisoString NamedType where
    toMisoString (NamedType name) = toMisoString name

-- | A GraphQL 'ListType'
-- https://spec.graphql.org/draft/#ListType
instance ToMisoString ListType where
    toMisoString (ListType type') = enclosed "[" "]" $ toMisoString type'

-- | A GraphQL 'NonNullType'
-- https://spec.graphql.org/draft/#NonNullType
instance ToMisoString NonNullType where
    toMisoString (NonNullTypeNamed namedType) = toMisoString namedType <> "!"
    toMisoString (NonNullTypeList listType) = toMisoString listType <> "!"

-- | The GraphQL 'Directives' type
-- https://spec.graphql.org/draft/#Directives
instance ToMisoString Directives where
    toMisoString = unwords'

-- | A GraphQL 'Directive'
-- https://spec.graphql.org/draft/#Directive
instance ToMisoString Directive where
    toMisoString (Directive name arguments) =
        mconcat
            [ "@"
            , toMisoString name
            , maybe "" toMisoString arguments
            ]

-- | A GraphQL 'TypeSystemDefinition'
-- https://spec.graphql.org/draft/#TypeSystemDefinition
instance ToMisoString TypeSystemDefinition where
    toMisoString (DefinitionSchema schemaDefinition) = toMisoString schemaDefinition
    toMisoString (DefinitionType typeDefinition) = toMisoString typeDefinition
    toMisoString (DefinitionDirective directiveDefinition) = toMisoString directiveDefinition

-- | A GraphQL 'TypeSystemExtension'
-- https://spec.graphql.org/draft/#TypeSystemExtension
instance ToMisoString TypeSystemExtension where
    toMisoString (ExtensionSchema schemaExtension) = toMisoString schemaExtension
    toMisoString (ExtensionType typeExtension) = toMisoString typeExtension

-- | A GraphQL 'SchemaDefinition'
-- https://spec.graphql.org/draft/#SchemaDefinition
instance ToMisoString SchemaDefinition where
    toMisoString (SchemaDefinition desc directives rootOperationTypeDefinitions) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just "schema"
              , toMisoString <$> directives
              , Just $ toMisoString rootOperationTypeDefinitions
              ]

-- | A GraphQL 'SchemaExtension'
-- https://spec.graphql.org/draft/#SchemaExtension
instance ToMisoString SchemaExtension where
    toMisoString (SchemaExtension directives rootOperationTypeDefinitions) =
        unwords
            . catMaybes
            $ [ Just "extend"
              , Just "schema"
              , toMisoString <$> directives
              , Just $ toMisoString rootOperationTypeDefinitions
              ]

-- | List of 'RootOperationTypeDefinition'
instance ToMisoString RootOperationTypeDefinitions where
    toMisoString = enclosed "{" "}" . unwords'

-- | https://spec.graphql.org/draft/#RootOperationTypeDefinition
instance ToMisoString RootOperationTypeDefinition where
    toMisoString (RootOperationTypeDefinition operationType namedType) =
        toMisoString operationType <> ":" <> toMisoString namedType

-- | A GraphQL 'Description'
-- https://spec.graphql.org/draft/#Description
instance ToMisoString Description where
    toMisoString (Description stringValue) = toMisoString stringValue

-- | A GraphQL 'TypeDefinition'
-- https://spec.graphql.org/draft/#TypeDefinition
instance ToMisoString TypeDefinition where
    toMisoString (DefinitionScalarType scalarTypeDefinition) = toMisoString scalarTypeDefinition
    toMisoString (DefinitionObjectType objectTypeDefinition) = toMisoString objectTypeDefinition
    toMisoString (DefinitionInterfaceType interfaceTypeDefinition) = toMisoString interfaceTypeDefinition
    toMisoString (DefinitionUnionType unionTypeDefinition) = toMisoString unionTypeDefinition
    toMisoString (DefinitionEnumType enumTypeDefinition) = toMisoString enumTypeDefinition
    toMisoString (DefinitionInputObjectType inputObjectTypeDefinition) = toMisoString inputObjectTypeDefinition

-- | A GraphQL 'TypeExtension'
-- https://spec.graphql.org/draft/#TypeExtension
instance ToMisoString TypeExtension where
    toMisoString (ExtensionScalarType scalarTypeExtension) = toMisoString scalarTypeExtension
    toMisoString (ExtensionObjectType objectTypeExtension) = toMisoString objectTypeExtension
    toMisoString (ExtensionInterfaceType interfaceTypeExtension) = toMisoString interfaceTypeExtension
    toMisoString (ExtensionUnionType unionTypeExtension) = toMisoString unionTypeExtension
    toMisoString (ExtensionEnumType enumTypeExtension) = toMisoString enumTypeExtension
    toMisoString (ExtensionInputObjectType inputObjectTypeExtension) = toMisoString inputObjectTypeExtension

-- | A GraphQL 'ScalarTypeDefinition'
-- https://spec.graphql.org/draft/#ScalarTypeDefinition
instance ToMisoString ScalarTypeDefinition where
    toMisoString (ScalarTypeDefinition desc name directives) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just "scalar"
              , Just $ toMisoString name
              , toMisoString <$> directives
              ]

-- | A GraphQL 'ScalarTypeExtension'
-- https://spec.graphql.org/draft/#ScalarTypeExtension
instance ToMisoString ScalarTypeExtension where
    toMisoString (ScalarTypeExtension name directives) =
        unwords
            . catMaybes
            $ [ Just "extend"
              , Just "scalar"
              , Just $ toMisoString name
              , toMisoString <$> directives
              ]

-- | A GraphQL 'ObjectTypeDefinition'
-- https://spec.graphql.org/draft/#ObjectTypeDefinition
instance ToMisoString ObjectTypeDefinition where
    toMisoString (ObjectTypeDefinition desc name implementsInterfaces directives fieldsDefinition) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just "type"
              , Just $ toMisoString name
              , toMisoString <$> implementsInterfaces
              , toMisoString <$> directives
              , toMisoString <$> fieldsDefinition
              ]

-- | A GraphQL 'ObjectTypeExtension'
-- https://spec.graphql.org/draft/#ObjectTypeExtension
instance ToMisoString ObjectTypeExtension where
    toMisoString (ObjectTypeExtension name implementsInterfaces directives fieldsDefinition) =
        unwords
            . catMaybes
            $ [ Just "extend"
              , Just "type"
              , Just $ toMisoString name
              , toMisoString <$> implementsInterfaces
              , toMisoString <$> directives
              , toMisoString <$> fieldsDefinition
              ]

-- | A GraphQL 'ImplementsInterfaces'
-- https://spec.graphql.org/draft/#ImplementsInterfaces
instance ToMisoString ImplementsInterfaces where
    toMisoString (ImplementsInterfaces types) =
        "implements " <> intercalate " & " (toMisoString <$> toList types)

-- | A GraphQL 'FieldsDefinition'
-- https://spec.graphql.org/draft/#FieldsDefinitionn
instance ToMisoString FieldsDefinition where
    toMisoString (FieldsDefinition fields) = enclosed "{" "}" $ unwords' fields

-- | A GraphQL 'FieldDefinition'
-- https://spec.graphql.org/draft/#FieldDefinition
instance ToMisoString FieldDefinition where
    toMisoString (FieldDefinition desc name argumentsDefinition type' directives) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just $ toMisoString name
              , toMisoString <$> argumentsDefinition
              , Just ":"
              , Just $ toMisoString type'
              , toMisoString <$> directives
              ]

-- | https://spec.graphql.org/draft/#ArgumentsDefinition
instance ToMisoString ArgumentsDefinition where
    toMisoString (ArgumentsDefinition args) = enclosed "(" ")" $ unwords' args

-- | https://spec.graphql.org/draft/#InputValueDefinition
instance ToMisoString InputValueDefinition where
    toMisoString (InputValueDefinition desc name type' defaultValue directives) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just $ toMisoString name
              , Just ":"
              , Just $ toMisoString type'
              , toMisoString <$> defaultValue
              , toMisoString <$> directives
              ]

-- | https://spec.graphql.org/draft/#InterfaceTypeDefinition
instance ToMisoString InterfaceTypeDefinition where
    toMisoString
        ( InterfaceTypeDefinition
                desc
                name
                implementsInterfaces
                directives
                fieldsDefinition
            ) =
            withDesc desc
                . unwords
                . catMaybes
                $ [ Just "interface"
                  , Just $ toMisoString name
                  , toMisoString <$> implementsInterfaces
                  , toMisoString <$> directives
                  , toMisoString <$> fieldsDefinition
                  ]

-- | https://spec.graphql.org/draft/#InterfaceTypeExtension
instance ToMisoString InterfaceTypeExtension where
    toMisoString (InterfaceTypeExtension name implementsInterfaces directives fieldsDefinition) =
        unwords
            . catMaybes
            $ [ Just "extend"
              , Just "interface"
              , Just $ toMisoString name
              , toMisoString <$> implementsInterfaces
              , toMisoString <$> directives
              , toMisoString <$> fieldsDefinition
              ]

-- | https://spec.graphql.org/draft/#UnionTypeDefinition
instance ToMisoString UnionTypeDefinition where
    toMisoString (UnionTypeDefinition desc name directives unionMemberTypes) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just "union"
              , Just $ toMisoString name
              , toMisoString <$> directives
              , toMisoString <$> unionMemberTypes
              ]

-- | https://spec.graphql.org/draft/#UnionMemberTypes
instance ToMisoString UnionMemberTypes where
    toMisoString (UnionMemberTypes types) = intercalate " | " $ toMisoString <$> toList types

-- | https://spec.graphql.org/draft/#UnionTypeExtension
instance ToMisoString UnionTypeExtension where
    toMisoString (UnionTypeExtension name directives unionMemberTypes) =
        unwords
            . catMaybes
            $ [ Just "extend"
              , Just "union"
              , Just $ toMisoString name
              , toMisoString <$> directives
              , toMisoString <$> unionMemberTypes
              ]

-- | https://spec.graphql.org/draft/#EnumTypeDefinition
instance ToMisoString EnumTypeDefinition where
    toMisoString (EnumTypeDefinition desc name directives enumValuesDefinition) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just "enum"
              , Just $ toMisoString name
              , toMisoString <$> directives
              , toMisoString <$> enumValuesDefinition
              ]

-- | https://spec.graphql.org/draft/#EnumValuesDefinition
instance ToMisoString EnumValuesDefinition where
    toMisoString (EnumValuesDefinition values) = enclosed "{" "}" $ unwords' values

-- | https://spec.graphql.org/draft/#EnumValueDefinition
instance ToMisoString EnumValueDefinition where
    toMisoString (EnumValueDefinition desc enumValue directives) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just $ toMisoString enumValue
              , toMisoString <$> directives
              ]

-- | https://spec.graphql.org/draft/#EnumTypeExtension
instance ToMisoString EnumTypeExtension where
    toMisoString (EnumTypeExtension name directives enumValuesDefinition) =
        unwords
            . catMaybes
            $ [ Just "extend"
              , Just "enum"
              , Just $ toMisoString name
              , toMisoString <$> directives
              , toMisoString <$> enumValuesDefinition
              ]

-- | InputObjectTypeDefinition
-- https://spec.graphql.org/draft/#InputObjectTypeDefinition
instance ToMisoString InputObjectTypeDefinition where
    toMisoString (InputObjectTypeDefinition desc name directives inputFieldsDefinition) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just "input"
              , Just $ toMisoString name
              , toMisoString <$> directives
              , toMisoString <$> inputFieldsDefinition
              ]

-- | InputFieldsDefinition
-- https://spec.graphql.org/draft/#InputFieldsDefinition
instance ToMisoString InputFieldsDefinition where
    toMisoString (InputFieldsDefinition fields) = enclosed "{" "}" $ unwords' fields

-- | InputObjectTypeExtension
-- https://spec.graphql.org/draft/#InputObjectTypeExtension
instance ToMisoString InputObjectTypeExtension where
    toMisoString (InputObjectTypeExtension name directives inputFieldsDefinition) =
        unwords
            . catMaybes
            $ [ Just "extend"
              , Just "input"
              , Just $ toMisoString name
              , toMisoString <$> directives
              , toMisoString <$> inputFieldsDefinition
              ]

-- | Directive definition
-- https://spec.graphql.org/draft/#DirectiveDefinition
instance ToMisoString DirectiveDefinition where
    toMisoString (DirectiveDefinition desc name argumentsDefinition repeatable directiveLocations) =
        withDesc desc
            . unwords
            . catMaybes
            $ [ Just "directive"
              , Just $ "@" <> toMisoString name
              , toMisoString <$> argumentsDefinition
              , bool Nothing (Just "repeatable") repeatable
              , Just "on"
              , Just $ toMisoString directiveLocations
              ]

-- | https://spec.graphql.org/draft/#DirectiveLocations
instance ToMisoString DirectiveLocations where
    toMisoString = intercalate " | " . fmap toMisoString . toList

-- | https://spec.graphql.org/draft/#DirectiveLocation
instance ToMisoString DirectiveLocation where
    toMisoString (LocationExecutableDirective executableDirectiveLocation) = toMisoString executableDirectiveLocation
    toMisoString (LocationTypeSystemDirective typeSystemDirectiveLocation) = toMisoString typeSystemDirectiveLocation

-- | https://spec.graphql.org/draft/#ExecutableDirectiveLocation
instance ToMisoString ExecutableDirectiveLocation where
    toMisoString QUERY = "QUERY"
    toMisoString MUTATION = "MUTATION"
    toMisoString SUBSCRIPTION = "SUBSCRIPTION"
    toMisoString FIELD = "FIELD"
    toMisoString FRAGMENT_DEFINITION = "FRAGMENT_DEFINITION"
    toMisoString FRAGMENT_SPREAD = "FRAGMENT_SPREAD"
    toMisoString INLINE_FRAGMENT = "INLINE_FRAGMENT"
    toMisoString VARIABLE_DEFINITION = "VARIABLE_DEFINITION"

-- | https://spec.graphql.org/draft/#TypeSystemDirectiveLocation
instance ToMisoString TypeSystemDirectiveLocation where
    toMisoString SCHEMA = "SCHEMA"
    toMisoString SCALAR = "SCALAR"
    toMisoString OBJECT = "OBJECT"
    toMisoString FIELD_DEFINITION = "FIELD_DEFINITION"
    toMisoString ARGUMENT_DEFINITION = "ARGUMENT_DEFINITION"
    toMisoString INTERFACE = "INTERFACE"
    toMisoString UNION = "UNION"
    toMisoString ENUM = "ENUM"
    toMisoString ENUM_VALUE = "ENUM_VALUE"
    toMisoString INPUT_OBJECT = "INPUT_OBJECT"
    toMisoString INPUT_FIELD_DEFINITION = "INPUT_FIELD_DEFINITION"

-- | A GraphQL 'Name'
-- https://spec.graphql.org/draft/#Name
instance ToMisoString Name where
    toMisoString (Name name) = name
