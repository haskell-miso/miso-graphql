{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Miso.GraphQL.AST
import Miso.GraphQL.Printer ()
import Miso.Prelude hiding (unlines)
import Miso.String qualified as MisoString
import Test.QuickCheck
import Data.Containers.ListUtils (nubOrdOn)
import Data.List.NonEmpty qualified as NonEmpty

chooseBoundedEnum :: (Bounded a, Enum a) => Gen a
chooseBoundedEnum = chooseEnum (minBound, maxBound)

anyStringChar :: Gen Char
anyStringChar = elements "abc"

anyMisoString :: Gen MisoString
anyMisoString = toMisoString <$> listOf anyStringChar

nonEmptyMisoString :: Gen MisoString
nonEmptyMisoString = toMisoString <$> listOf1 anyStringChar

anyMultiLineMisoString :: Gen MisoString
anyMultiLineMisoString = MisoString.unlines <$> listOf anyMisoString

nonEmptyMultiLineMisoString :: Gen MisoString
nonEmptyMultiLineMisoString = MisoString.unlines <$> listOf1 nonEmptyMisoString

uniqueOn :: (Arbitrary [a], Ord b) => (a -> b) -> Gen [a]
uniqueOn f = nubOrdOn f <$> arbitrary

uniqueOn1 :: (Arbitrary (NonEmpty a), Ord b) => (a -> b) -> Gen (NonEmpty a)
uniqueOn1 f = NonEmpty.fromList . nubOrdOn f . NonEmpty.toList <$> arbitrary

instance (Arbitrary a) => Arbitrary (NonEmpty a) where
    arbitrary = liftA2 (:|) arbitrary arbitrary

instance Arbitrary ExecutableDirectiveLocation where
    arbitrary = chooseBoundedEnum

instance Arbitrary TypeSystemDirectiveLocation where
    arbitrary = chooseBoundedEnum

instance Arbitrary StringValue where
    arbitrary =
        oneof
            [ SingleLineString <$> anyMisoString
            , BlockString <$> anyMultiLineMisoString
            ]

instance Arbitrary Document where
    arbitrary = Document <$> arbitrary

instance Arbitrary Definition where
    arbitrary =
        oneof
            [ DefinitionExecutable <$> arbitrary
            , DefinitionTypeSystem <$> arbitrary
            , ExtensionTypeSystem <$> arbitrary
            ]

instance Arbitrary ExecutableDefinition where
    arbitrary =
        oneof
            [ DefinitionOperation <$> arbitrary
            , DefinitionFragment <$> arbitrary
            ]

instance Arbitrary OperationDefinition where
    arbitrary =
        OperationDefinition
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary OperationType where
    arbitrary = chooseBoundedEnum

instance Arbitrary Selection where
    arbitrary =
        oneof
            [ SelectionField <$> arbitrary
            , SelectionFragmentSpread <$> arbitrary
            , SelectionInlineFragment <$> arbitrary
            ]

instance Arbitrary Field where
    arbitrary = Field <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Alias where
    arbitrary = Alias <$> arbitrary

instance {-# OVERLAPPING #-} Arbitrary Arguments where
    arbitrary = uniqueOn1 argumentName

instance Arbitrary Argument where
    arbitrary = Argument <$> arbitrary <*> arbitrary

instance Arbitrary FragmentSpread where
    arbitrary = FragmentSpread <$> arbitrary <*> arbitrary

instance Arbitrary InlineFragment where
    arbitrary = InlineFragment <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary FragmentDefinition where
    arbitrary =
        FragmentDefinition
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary FragmentName where
    arbitrary = FragmentName <$> arbitrary

instance Arbitrary TypeCondition where
    arbitrary = TypeCondition <$> arbitrary

instance Arbitrary Value where
    arbitrary =
        oneof
            [ ValueVariable <$> arbitrary
            , ValueInt <$> arbitrary
            , ValueFloat <$> arbitrary
            , ValueString <$> arbitrary
            , ValueBoolean <$> arbitrary
            , pure ValueNull
            , ValueEnum <$> arbitrary
            , ValueList <$> arbitrary
            , ValueObject <$> arbitrary
            ]

instance Arbitrary EnumValue where
    arbitrary = EnumValue <$> arbitrary

instance Arbitrary ObjectField where
    arbitrary = ObjectField <$> arbitrary <*> arbitrary

instance {-# OVERLAPPING #-} Arbitrary [ObjectField] where
    arbitrary = uniqueOn objectFieldName

instance Arbitrary VariableDefinition where
    arbitrary =
        VariableDefinition
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance {-# OVERLAPPING #-} Arbitrary VariablesDefinition where
    arbitrary = uniqueOn1 variableDefinitionName

instance Arbitrary Variable where
    arbitrary = Variable <$> arbitrary

instance Arbitrary DefaultValue where
    arbitrary = DefaultValue <$> arbitrary

instance Arbitrary Type where
    arbitrary =
        oneof
            [ TypeNamed <$> arbitrary
            , TypeList <$> arbitrary
            , TypeNonNull <$> arbitrary
            ]

instance Arbitrary NamedType where
    arbitrary = NamedType <$> arbitrary

instance Arbitrary ListType where
    arbitrary = ListType <$> arbitrary

instance Arbitrary NonNullType where
    arbitrary =
        oneof
            [ NonNullTypeNamed <$> arbitrary
            , NonNullTypeList <$> arbitrary
            ]

instance Arbitrary Directive where
    arbitrary = Directive <$> arbitrary <*> arbitrary

instance Arbitrary TypeSystemDefinition where
    arbitrary =
        oneof
            [ DefinitionSchema <$> arbitrary
            , DefinitionType <$> arbitrary
            , DefinitionDirective <$> arbitrary
            ]

instance Arbitrary TypeSystemExtension where
    arbitrary =
        oneof
            [ ExtensionSchema <$> arbitrary
            , ExtensionType <$> arbitrary
            ]

instance Arbitrary SchemaDefinition where
    arbitrary = SchemaDefinition <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SchemaExtension where
    arbitrary = SchemaExtension <$> arbitrary <*> arbitrary

instance {-# OVERLAPPING #-} Arbitrary RootOperationTypeDefinitions where
    arbitrary = uniqueOn1 rootOperationType

instance Arbitrary RootOperationTypeDefinition where
    arbitrary = RootOperationTypeDefinition <$> arbitrary <*> arbitrary

instance Arbitrary Description where
    arbitrary = Description <$> arbitrary

instance Arbitrary TypeDefinition where
    arbitrary =
        oneof
            [ DefinitionScalarType <$> arbitrary
            , DefinitionObjectType <$> arbitrary
            , DefinitionInterfaceType <$> arbitrary
            , DefinitionUnionType <$> arbitrary
            , DefinitionEnumType <$> arbitrary
            , DefinitionInputObjectType <$> arbitrary
            ]

instance Arbitrary TypeExtension where
    arbitrary =
        oneof
            [ ExtensionScalarType <$> arbitrary
            , ExtensionObjectType <$> arbitrary
            , ExtensionInterfaceType <$> arbitrary
            , ExtensionUnionType <$> arbitrary
            , ExtensionEnumType <$> arbitrary
            , ExtensionInputObjectType <$> arbitrary
            ]

instance Arbitrary ScalarTypeDefinition where
    arbitrary = ScalarTypeDefinition <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ScalarTypeExtension where
    arbitrary = ScalarTypeExtension <$> arbitrary <*> arbitrary

instance Arbitrary ObjectTypeDefinition where
    arbitrary =
        ObjectTypeDefinition
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary ObjectTypeExtension where
    arbitrary = ObjectTypeExtension <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ImplementsInterfaces where
    arbitrary = ImplementsInterfaces <$> arbitrary

instance Arbitrary FieldsDefinition where
    arbitrary = FieldsDefinition <$> uniqueOn1 fieldDefinitionName

instance Arbitrary FieldDefinition where
    arbitrary =
        FieldDefinition
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary ArgumentsDefinition where
    arbitrary = ArgumentsDefinition <$> uniqueOn1 inputValueDefinitionName

instance Arbitrary InputValueDefinition where
    arbitrary =
        InputValueDefinition
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary InterfaceTypeDefinition where
    arbitrary =
        InterfaceTypeDefinition
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary InterfaceTypeExtension where
    arbitrary =
        InterfaceTypeExtension <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UnionTypeDefinition where
    arbitrary = UnionTypeDefinition <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UnionMemberTypes where
    arbitrary = UnionMemberTypes <$> arbitrary

instance Arbitrary UnionTypeExtension where
    arbitrary = UnionTypeExtension <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary EnumTypeDefinition where
    arbitrary = EnumTypeDefinition <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary EnumValuesDefinition where
    arbitrary = EnumValuesDefinition <$> uniqueOn1 enumValueDefinitionName

instance Arbitrary EnumValueDefinition where
    arbitrary = EnumValueDefinition <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary EnumTypeExtension where
    arbitrary = EnumTypeExtension <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary InputObjectTypeDefinition where
    arbitrary =
        InputObjectTypeDefinition
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary InputFieldsDefinition where
    arbitrary = InputFieldsDefinition <$> uniqueOn1 inputValueDefinitionName

instance Arbitrary InputObjectTypeExtension where
    arbitrary = InputObjectTypeExtension <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DirectiveDefinition where
    arbitrary =
        DirectiveDefinition
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary DirectiveLocation where
    arbitrary =
        oneof
            [ LocationExecutableDirective <$> arbitrary
            , LocationTypeSystemDirective <$> arbitrary
            ]

instance Arbitrary Name where
    arbitrary = Name <$> nonEmptyMisoString
