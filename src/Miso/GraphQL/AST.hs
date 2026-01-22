module Miso.GraphQL.AST where

import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Miso.Prelude

-- | https://spec.graphql.org/draft/#ExecutableDirectiveLocation
data ExecutableDirectiveLocation
    = QUERY
    | MUTATION
    | SUBSCRIPTION
    | FIELD
    | FRAGMENT_DEFINITION
    | FRAGMENT_SPREAD
    | INLINE_FRAGMENT
    | VARIABLE_DEFINITION
    deriving stock (Show, Eq, Generic, Bounded, Enum)

-- | https://spec.graphql.org/draft/#TypeSystemDirectiveLocation
data TypeSystemDirectiveLocation
    = SCHEMA
    | SCALAR
    | OBJECT
    | FIELD_DEFINITION
    | ARGUMENT_DEFINITION
    | INTERFACE
    | UNION
    | ENUM
    | ENUM_VALUE
    | INPUT_OBJECT
    | INPUT_FIELD_DEFINITION
    deriving stock (Show, Eq, Generic, Bounded, Enum)

-- | https://spec.graphql.org/draft/#StringValue
data StringValue
    = SingleLineString MisoString
    | BlockString MisoString
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'Document'
-- https://spec.graphql.org/draft/#Document
newtype Document = Document (NonEmpty Definition)
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'Definition'
-- https://spec.graphql.org/draft/#Definition
data Definition
    = DefinitionExecutable ExecutableDefinition
    | DefinitionTypeSystem TypeSystemDefinition
    | ExtensionTypeSystem TypeSystemExtension
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'ExecutableDefinition'
-- https://spec.graphql.org/draft/#ExecutableDefinition
data ExecutableDefinition
    = DefinitionOperation OperationDefinition
    | DefinitionFragment FragmentDefinition
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'OperationDefinition'
-- https://spec.graphql.org/draft/#OperationDefinition
data OperationDefinition
    = AnonymousQuery SelectionSet
    | OperationDefinition
        (Maybe Description)
        OperationType
        (Maybe Name)
        (Maybe VariablesDefinition)
        (Maybe Directives)
        SelectionSet
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'Operation' type
-- https://spec.graphql.org/draft/#OperationType
data OperationType
    = Query
    | Mutation
    | Subscription
    deriving stock (Show, Eq, Ord, Generic, Bounded, Enum)

-- | A GraphQL 'SelectionSet'
-- https://spec.graphql.org/draft/#SelectionSet
type SelectionSet = NonEmpty Selection

-- | A GraphQL 'Selection' type
-- https://spec.graphql.org/draft/#Selection
data Selection
    = SelectionField Field
    | SelectionFragmentSpread FragmentSpread
    | SelectionInlineFragment InlineFragment
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'Field' type
-- https://spec.graphql.org/draft/#Field
data Field
    = Field
        (Maybe Alias)
        Name
        (Maybe Arguments)
        (Maybe Directives)
        (Maybe SelectionSet)
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'Alias'
-- https://spec.graphql.org/draft/#Alias
newtype Alias = Alias Name
    deriving stock (Show, Eq, Ord, Generic)

-- | GraphQL 'Arguments'
-- https://spec.graphql.org/draft/#Arguments
type Arguments = NonEmpty Argument

-- | A GraphQL 'Argument'
-- https://spec.graphql.org/draft/#Arguments
data Argument = Argument Name Value
    deriving stock (Show, Eq, Generic)

argumentName :: Argument -> Name
argumentName (Argument name _) = name

-- | GraphQL 'FragmentSpread' type
-- https://spec.graphql.org/draft/#FragmentSpread
data FragmentSpread = FragmentSpread FragmentName (Maybe Directives)
    deriving stock (Show, Eq, Generic)

-- | GraphQL 'InlineFragment' type
-- https://spec.graphql.org/draft/#InlineFragment
data InlineFragment
    = InlineFragment (Maybe TypeCondition) (Maybe Directives) SelectionSet
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'FragmentDefinition'
-- https://spec.graphql.org/draft/#FragmentDefinition
data FragmentDefinition
    = FragmentDefinition
        (Maybe Description)
        FragmentName
        TypeCondition
        (Maybe Directives)
        SelectionSet
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'FragmentName'
-- https://spec.graphql.org/draft/#FragmentName
newtype FragmentName = FragmentName Name
    deriving stock (Generic)
    deriving newtype (Show, Eq, Ord, Monoid, Semigroup)

-- | A GraphQL 'TypeCondition'
-- https://spec.graphql.org/draft/#TypeCondition
newtype TypeCondition = TypeCondition NamedType
    deriving stock (Generic)
    deriving newtype (Show, Eq, Monoid, Semigroup)

-- | A GraphQL 'Value'
-- https://spec.graphql.org/draft/#Value
data Value
    = ValueVariable Variable
    | ValueInt Int
    | ValueFloat Double
    | ValueString StringValue
    | ValueBoolean Bool
    | ValueNull
    | ValueEnum EnumValue
    | ValueList [Value]
    | ValueObject [ObjectField]
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'EnumValue'
-- https://spec.graphql.org/draft/#EnumValue
newtype EnumValue = EnumValue Name
    deriving stock (Generic)
    deriving newtype (Show, Eq, Ord, Monoid, Semigroup)

enumValueName :: EnumValue -> Name
enumValueName (EnumValue name) = name

-- | A GraphQL 'ObjectField'
-- https://spec.graphql.org/draft/#ObjectField
data ObjectField = ObjectField Name Value
    deriving stock (Show, Eq, Generic)

objectFieldName :: ObjectField -> Name
objectFieldName (ObjectField name _) = name

-- | GraphQL 'VariablesDefinition'
-- https://spec.graphql.org/draft/#VariablesDefinition
type VariablesDefinition = NonEmpty VariableDefinition

-- | A GraphQL 'VariableDefinition'
-- https://spec.graphql.org/draft/#VariableDefinition
data VariableDefinition
    = VariableDefinition
        (Maybe Description)
        Variable
        Type
        (Maybe DefaultValue)
        (Maybe Directives)
    deriving stock (Show, Eq, Generic)

variableDefinitionName :: VariableDefinition -> Name
variableDefinitionName (VariableDefinition _ var _ _ _) = variableName var

-- | A GraphQL 'Variable'
-- https://spec.graphql.org/draft/#Variable
newtype Variable = Variable Name
    deriving stock (Generic)
    deriving newtype (Show, Eq, Ord, Monoid, Semigroup)

variableName :: Variable -> Name
variableName (Variable name) = name

-- | A GraphQL 'DefaultValue'
-- https://spec.graphql.org/draft/#DefaultValue
newtype DefaultValue = DefaultValue Value
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'Type'
-- https://spec.graphql.org/draft/#Type
data Type
    = TypeNamed NamedType
    | TypeList ListType
    | TypeNonNull NonNullType
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'NamedType'
-- https://spec.graphql.org/draft/#NamedType
newtype NamedType = NamedType Name
    deriving stock (Generic)
    deriving newtype (Show, Eq, Ord, Monoid, Semigroup)

namedTypeName :: NamedType -> Name
namedTypeName (NamedType name) = name

-- | A GraphQL 'ListType'
-- https://spec.graphql.org/draft/#ListType
newtype ListType = ListType Type
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'NonNullType'
-- https://spec.graphql.org/draft/#NonNullType
data NonNullType
    = NonNullTypeNamed NamedType
    | NonNullTypeList ListType
    deriving stock (Show, Eq, Generic)

-- | The GraphQL 'Directives' type
-- https://spec.graphql.org/draft/#Directives
type Directives = NonEmpty Directive

-- | A GraphQL 'Directive'
-- https://spec.graphql.org/draft/#Directive
data Directive = Directive Name (Maybe Arguments)
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'TypeSystemDefinition'
-- https://spec.graphql.org/draft/#TypeSystemDefinition
data TypeSystemDefinition
    = DefinitionSchema SchemaDefinition
    | DefinitionType TypeDefinition
    | DefinitionDirective DirectiveDefinition
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'TypeSystemExtension'
-- https://spec.graphql.org/draft/#TypeSystemExtension
data TypeSystemExtension
    = ExtensionSchema SchemaExtension
    | ExtensionType TypeExtension
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'SchemaDefinition'
-- https://spec.graphql.org/draft/#SchemaDefinition
data SchemaDefinition
    = SchemaDefinition
        (Maybe Description)
        (Maybe Directives)
        RootOperationTypeDefinitions
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'SchemaExtension'
-- https://spec.graphql.org/draft/#SchemaExtension
data SchemaExtension
    = SchemaExtension (Maybe Directives) (Maybe RootOperationTypeDefinitions)
    deriving stock (Show, Eq, Generic)

-- | List of 'RootOperationTypeDefinition'
type RootOperationTypeDefinitions = NonEmpty RootOperationTypeDefinition

-- | https://spec.graphql.org/draft/#RootOperationTypeDefinition
data RootOperationTypeDefinition
    = RootOperationTypeDefinition OperationType NamedType
    deriving stock (Show, Eq, Generic)

rootOperationType :: RootOperationTypeDefinition -> OperationType
rootOperationType (RootOperationTypeDefinition operationType _) = operationType

-- | A GraphQL 'Description'
-- https://spec.graphql.org/draft/#Description
newtype Description = Description StringValue
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'TypeDefinition'
-- https://spec.graphql.org/draft/#TypeDefinition
data TypeDefinition
    = DefinitionScalarType ScalarTypeDefinition
    | DefinitionObjectType ObjectTypeDefinition
    | DefinitionInterfaceType InterfaceTypeDefinition
    | DefinitionUnionType UnionTypeDefinition
    | DefinitionEnumType EnumTypeDefinition
    | DefinitionInputObjectType InputObjectTypeDefinition
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'TypeExtension'
-- https://spec.graphql.org/draft/#TypeExtension
data TypeExtension
    = ExtensionScalarType ScalarTypeExtension
    | ExtensionObjectType ObjectTypeExtension
    | ExtensionInterfaceType InterfaceTypeExtension
    | ExtensionUnionType UnionTypeExtension
    | ExtensionEnumType EnumTypeExtension
    | ExtensionInputObjectType InputObjectTypeExtension
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'ScalarTypeDefinition'
-- https://spec.graphql.org/draft/#ScalarTypeDefinition
data ScalarTypeDefinition
    = ScalarTypeDefinition (Maybe Description) Name (Maybe Directives)
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'ScalarTypeExtension'
-- https://spec.graphql.org/draft/#ScalarTypeExtension
data ScalarTypeExtension = ScalarTypeExtension Name Directives
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'ObjectTypeDefinition'
-- https://spec.graphql.org/draft/#ObjectTypeDefinition
data ObjectTypeDefinition
    = ObjectTypeDefinition
        (Maybe Description)
        Name
        (Maybe ImplementsInterfaces)
        (Maybe Directives)
        (Maybe FieldsDefinition)
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'ObjectTypeExtension'
-- https://spec.graphql.org/draft/#ObjectTypeExtension
data ObjectTypeExtension
    = ObjectTypeExtension
        Name
        (Maybe ImplementsInterfaces)
        (Maybe Directives)
        (Maybe FieldsDefinition)
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'ImplementsInterfaces'
-- https://spec.graphql.org/draft/#ImplementsInterfaces
newtype ImplementsInterfaces = ImplementsInterfaces (NonEmpty NamedType)
    deriving stock (Generic)
    deriving newtype (Show, Eq, Semigroup)

-- | A GraphQL 'FieldsDefinition'
-- https://spec.graphql.org/draft/#FieldsDefinitionn
newtype FieldsDefinition = FieldsDefinition (NonEmpty FieldDefinition)
    deriving stock (Generic)
    deriving newtype (Show, Eq, Semigroup)

-- | A GraphQL 'FieldDefinition'
-- https://spec.graphql.org/draft/#FieldDefinition
data FieldDefinition
    = FieldDefinition
        (Maybe Description)
        Name
        (Maybe ArgumentsDefinition)
        Type
        (Maybe Directives)
    deriving stock (Show, Eq, Generic)

fieldDefinitionName :: FieldDefinition -> Name
fieldDefinitionName (FieldDefinition _ name _ _ _) = name

-- | https://spec.graphql.org/draft/#ArgumentsDefinition
newtype ArgumentsDefinition = ArgumentsDefinition (NonEmpty InputValueDefinition)
    deriving stock (Generic)
    deriving newtype (Show, Eq, Semigroup)

-- | https://spec.graphql.org/draft/#InputValueDefinition
data InputValueDefinition
    = InputValueDefinition
        (Maybe Description)
        Name
        Type
        (Maybe DefaultValue)
        (Maybe Directives)
    deriving stock (Show, Eq, Generic)

inputValueDefinitionName :: InputValueDefinition -> Name
inputValueDefinitionName (InputValueDefinition _ name _ _ _) = name

-- | https://spec.graphql.org/draft/#InterfaceTypeDefinition
data InterfaceTypeDefinition
    = InterfaceTypeDefinition
        (Maybe Description)
        Name
        (Maybe ImplementsInterfaces)
        (Maybe Directives)
        (Maybe FieldsDefinition)
    deriving stock (Show, Eq, Generic)

-- | https://spec.graphql.org/draft/#InterfaceTypeExtension
data InterfaceTypeExtension
    = InterfaceTypeExtension
        Name
        (Maybe ImplementsInterfaces)
        (Maybe Directives)
        (Maybe FieldsDefinition)
    deriving stock (Show, Eq, Generic)

-- | https://spec.graphql.org/draft/#UnionTypeDefinition
data UnionTypeDefinition
    = UnionTypeDefinition
        (Maybe Description)
        Name
        (Maybe Directives)
        (Maybe UnionMemberTypes)
    deriving stock (Show, Eq, Generic)

-- | https://spec.graphql.org/draft/#UnionMemberTypes
newtype UnionMemberTypes = UnionMemberTypes (NonEmpty NamedType)
    deriving stock (Generic)
    deriving newtype (Show, Eq, Semigroup)

-- | https://spec.graphql.org/draft/#UnionTypeExtension
data UnionTypeExtension
    = UnionTypeExtension Name (Maybe Directives) (Maybe UnionMemberTypes)
    deriving stock (Show, Eq, Generic)

-- | https://spec.graphql.org/draft/#EnumTypeDefinition
data EnumTypeDefinition
    = EnumTypeDefinition
        (Maybe Description)
        Name
        (Maybe Directives)
        (Maybe EnumValuesDefinition)
    deriving stock (Show, Eq, Generic)

-- | https://spec.graphql.org/draft/#EnumValuesDefinition
newtype EnumValuesDefinition = EnumValuesDefinition (NonEmpty EnumValueDefinition)
    deriving stock (Generic)
    deriving newtype (Show, Eq, Semigroup)

-- | https://spec.graphql.org/draft/#EnumValueDefinition
data EnumValueDefinition
    = EnumValueDefinition (Maybe Description) EnumValue (Maybe Directives)
    deriving stock (Show, Eq, Generic)

enumValueDefinitionName :: EnumValueDefinition -> Name
enumValueDefinitionName (EnumValueDefinition _ value _) = enumValueName value

-- | https://spec.graphql.org/draft/#EnumTypeExtension
data EnumTypeExtension
    = EnumTypeExtension Name (Maybe Directives) (Maybe EnumValuesDefinition)
    deriving stock (Show, Eq, Generic)

-- | InputObjectTypeDefinition
-- https://spec.graphql.org/draft/#InputObjectTypeDefinition
data InputObjectTypeDefinition
    = InputObjectTypeDefinition
        (Maybe Description)
        Name
        (Maybe Directives)
        (Maybe InputFieldsDefinition)
    deriving stock (Show, Eq, Generic)

-- | InputFieldsDefinition
-- https://spec.graphql.org/draft/#InputFieldsDefinition
newtype InputFieldsDefinition = InputFieldsDefinition (NonEmpty InputValueDefinition)
    deriving stock (Generic)
    deriving newtype (Show, Eq, Semigroup)

-- | InputObjectTypeExtension
-- https://spec.graphql.org/draft/#InputObjectTypeExtension
data InputObjectTypeExtension
    = InputObjectTypeExtension Name (Maybe Directives) (Maybe InputFieldsDefinition)
    deriving stock (Show, Eq, Generic)

-- | Directive definition
-- https://spec.graphql.org/draft/#DirectiveDefinition
data DirectiveDefinition
    = DirectiveDefinition
        (Maybe Description)
        Name
        (Maybe ArgumentsDefinition)
        Bool
        DirectiveLocations
    deriving stock (Show, Eq, Generic)

-- | https://spec.graphql.org/draft/#DirectiveLocations
type DirectiveLocations = NonEmpty DirectiveLocation

-- | https://spec.graphql.org/draft/#DirectiveLocation
data DirectiveLocation
    = LocationExecutableDirective ExecutableDirectiveLocation
    | LocationTypeSystemDirective TypeSystemDirectiveLocation
    deriving stock (Show, Eq, Generic)

-- | A GraphQL 'Name'
-- https://spec.graphql.org/draft/#Name
newtype Name = Name MisoString
    deriving stock (Generic)
    deriving newtype (Show, Eq, Ord, Monoid, Semigroup)
