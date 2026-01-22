module Miso.GraphQL.Parser where

import Control.Applicative (Alternative (empty, many), optional, (<|>))
import Control.Monad (guard)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty, some1)
import Data.List.NonEmpty qualified as NonEmpty
import Debug.Trace
import Miso.GraphQL.AST
import Miso.GraphQL.Lexer (Token (..), lex)
import Miso.Prelude hiding (lex)
import Miso.Util.Lexer (Lexer)
import Miso.Util.Parser

parse'
    :: Lexer [token]
    -> Parser token a
    -> MisoString
    -> Either (ParseError a token) a
parse' lexer parser = trace "OH BOY HERE WE GO PARSING" $ either (Left . LexicalError) (parse parser) . lex lexer

traceParser :: (Show token, Show a) => String -> ParserT () [token] [] a -> ParserT () [token] [] a
traceParser s p = do
    traceM . (("parser> " <> s <> " ") <>) . show =<< optional peek
    a <-
        p <|> do
            traceM $ "!parser " <> s
            empty
    traceM $ "<parser " <> s <> ": " <> show a
    pure a

punctuator :: Char -> Parser Token ()
punctuator c = void . traceParser ("punctuator " <> show c) . token_ . TokenPunctuator $ c

keyword :: MisoString -> Parser Token ()
keyword k = void . traceParser ("keyword " <> show k) . token_ . TokenName . Name $ k

enclosed' :: Char -> Char -> Parser Token a -> Parser Token a
enclosed' a b = enclosed (punctuator a) (punctuator b)

uniqueOn
    :: (Foldable f, Ord b)
    => (a -> b)
    -> Parser Token (f a)
    -> Parser Token (f a)
uniqueOn f p = do
    as <- p
    let numAs = length as
        numBs = length . nubOrdOn f . toList $ as
    guard $ numAs == numBs
    pure as

some1UniqueOn
    :: (Ord b)
    => (a -> b)
    -> Parser Token a
    -> Parser Token (NonEmpty a)
some1UniqueOn f = uniqueOn f . some1

sepBy1' :: Char -> Parser Token a -> Parser Token (NonEmpty a)
sepBy1' = fmap (fmap NonEmpty.fromList) . sepBy1 . punctuator

lookaheadCantBe :: (Eq token, Show token, Show a) => token -> a -> Parser token a
lookaheadCantBe token a = traceParser ("lookaheadCantBe " <> show a) $ do
    guard . (Just token /=) =<< optional peek
    pure a

lookaheadCantBe' :: Show a => Char -> a -> Parser Token a
lookaheadCantBe' = lookaheadCantBe . TokenPunctuator

nextToken :: Show a => ParserT () [a] [] a
nextToken = traceParser "nextToken" $ satisfy $ const True

-- | https://spec.graphql.org/draft/#Document
document :: Parser Token Document
document = traceParser "document" $ Document <$> some1 definition

-- | https://spec.graphql.org/draft/#Definition
definition :: Parser Token Definition
definition =
    traceParser "definition"
        $ oneOf
            [ DefinitionExecutable <$> executableDefinition
            , DefinitionTypeSystem <$> typeSystemDefinition
            , ExtensionTypeSystem <$> typeSystemExtension
            ]

-- https://spec.graphql.org/draft/#ExecutableDefinition
executableDefinition :: Parser Token ExecutableDefinition
executableDefinition =
    traceParser "executableDefinition"
        $ oneOf
            [ DefinitionOperation <$> operationDefinition
            , DefinitionFragment <$> fragmentDefinition
            ]

-- | A GraphQL 'OperationDefinition'
-- https://spec.graphql.org/draft/#OperationDefinition
operationDefinition :: Parser Token OperationDefinition
operationDefinition =
    traceParser "operationDefinition"
        $ oneOf
            [ AnonymousQuery <$> selectionSet
            , OperationDefinition
                <$> optional description
                <*> operationType
                <*> optional name
                <*> optional variablesDefinition
                <*> optional directives
                <*> selectionSet
            ]

-- | A GraphQL 'Operation' type
-- https://spec.graphql.org/draft/#OperationType
operationType :: Parser Token OperationType
operationType =
    traceParser "operationType"
        $ oneOf
            [ Query <$ keyword "query"
            , Mutation <$ keyword "mutation"
            , Subscription <$ keyword "subscription"
            ]

-- | A GraphQL 'SelectionSet'
-- https://spec.graphql.org/draft/#SelectionSet
selectionSet :: Parser Token SelectionSet
selectionSet = traceParser "selectionSet" $ enclosed' '{' '}' $ some1 selection

-- | A GraphQL 'Selection' type
-- https://spec.graphql.org/draft/#Selection
selection :: Parser Token Selection
selection =
    traceParser "selection"
        $ oneOf
            [ SelectionField <$> field
            , SelectionFragmentSpread <$> fragmentSpread
            , SelectionInlineFragment <$> inlineFragment
            ]

-- | A GraphQL 'Field' type
-- https://spec.graphql.org/draft/#Field
field :: Parser Token Field
field =
    traceParser "field"
        $ Field
        <$> optional alias
        <*> name
        <*> optional arguments
        <*> optional directives
        <*> optional selectionSet

-- | A GraphQL 'Alias'
-- https://spec.graphql.org/draft/#Alias
alias :: Parser Token Alias
alias = traceParser "alias" $ Alias <$> name <* punctuator ':'

-- | GraphQL 'Arguments'
-- https://spec.graphql.org/draft/#Arguments
arguments :: Parser Token Arguments
arguments =
    traceParser "arguments"
        $ enclosed' '(' ')'
        . uniqueOn argumentName
        $ some1 argument

-- | A GraphQL 'Argument'
-- https://spec.graphql.org/draft/#Arguments
argument :: Parser Token Argument
argument = traceParser "argument" $ Argument <$> name <* punctuator ':' <*> value

-- | GraphQL 'FragmentSpread' type
-- https://spec.graphql.org/draft/#FragmentSpread
fragmentSpread :: Parser Token FragmentSpread
fragmentSpread =
    traceParser "fragmentSpread"
        $ FragmentSpread
        <$ token_ TokenEllipsis
        <*> fragmentName
        <*> optional directives

-- | GraphQL 'InlineFragment' type
-- https://spec.graphql.org/draft/#InlineFragment
inlineFragment :: Parser Token InlineFragment
inlineFragment =
    traceParser "inlineFragment"
        $ InlineFragment
        <$ token_ TokenEllipsis
        <*> optional typeCondition
        <*> optional directives
        <*> selectionSet

-- | A GraphQL 'FragmentDefinition'
-- https://spec.graphql.org/draft/#FragmentDefinition
fragmentDefinition :: Parser Token FragmentDefinition
fragmentDefinition =
    traceParser "fragmentDefinition"
        $ FragmentDefinition
        <$> optional description
        <* keyword "fragment"
        <*> fragmentName
        <*> typeCondition
        <*> optional directives
        <*> selectionSet

-- | A GraphQL 'FragmentName'
-- https://spec.graphql.org/draft/#FragmentName
fragmentName :: Parser Token FragmentName
fragmentName = traceParser "fragmentName" $ FragmentName <$> nameButNot ["on"]

-- | A GraphQL 'TypeCondition'
-- https://spec.graphql.org/draft/#TypeCondition
typeCondition :: Parser Token TypeCondition
typeCondition = traceParser "typeCondition" $ TypeCondition <$ keyword "on" <*> namedType

-- | A GraphQL 'Value'
-- https://spec.graphql.org/draft/#Value
value :: Parser Token Value
value =
    traceParser "value"
        $ oneOf
            [ ValueVariable <$> variable
            , nextToken >>= \case
                TokenInt i -> pure $ ValueInt i
                TokenFloat f -> pure $ ValueFloat f
                TokenString s -> pure $ ValueString s
                _ -> empty
            , ValueBoolean True <$ keyword "true"
            , ValueBoolean False <$ keyword "false"
            , ValueNull <$ keyword "null"
            , ValueEnum <$> enumValue
            , ValueList <$> listValue
            , ValueObject <$> objectValue
            ]

-- | https://spec.graphql.org/draft/#ListValue
listValue :: Parser Token [Value]
listValue = traceParser "listValue" $ enclosed' '[' ']' $ many value

-- | https://spec.graphql.org/draft/#ObjectValue
objectValue :: Parser Token [ObjectField]
objectValue =
    traceParser "objectValue"
        $ enclosed' '{' '}'
        . uniqueOn objectFieldName
        $ many objectField

-- | A GraphQL 'EnumValue'
-- https://spec.graphql.org/draft/#EnumValue
enumValue :: Parser Token EnumValue
enumValue = traceParser "enumValue" $ EnumValue <$> nameButNot ["true", "false", "null"]

-- | A GraphQL 'ObjectField'
-- https://spec.graphql.org/draft/#ObjectField
objectField :: Parser Token ObjectField
objectField = traceParser "objectField" $ ObjectField <$> name <* punctuator ':' <*> value

-- | GraphQL 'VariablesDefinition'
-- https://spec.graphql.org/draft/#VariablesDefinition
variablesDefinition :: Parser Token VariablesDefinition
variablesDefinition =
    traceParser "variablesDefinition"
        $ enclosed' '(' ')'
        . uniqueOn variableDefinitionName
        $ some1 variableDefinition

-- | A GraphQL 'VariableDefinition'
-- https://spec.graphql.org/draft/#VariableDefinition
variableDefinition :: Parser Token VariableDefinition
variableDefinition =
    traceParser "variableDefinition"
        $ VariableDefinition
        <$> optional description
        <*> variable
        <* punctuator ':'
        <*> type'
        <*> optional defaultValue
        <*> optional directives

-- | A GraphQL 'Variable'
-- https://spec.graphql.org/draft/#Variable
variable :: Parser Token Variable
variable = traceParser "variable" $ Variable <$ punctuator '$' <*> name

-- | A GraphQL 'DefaultValue'
-- https://spec.graphql.org/draft/#DefaultValue
defaultValue :: Parser Token DefaultValue
defaultValue = traceParser "defaultValue" $ DefaultValue <$ punctuator '=' <*> value

-- | A GraphQL 'Type'
-- https://spec.graphql.org/draft/#Type
type' :: Parser Token Type
type' =
    traceParser "type'"
        $ oneOf
            [ TypeNonNull <$> nonNullType
            , TypeList <$> listType
            , TypeNamed <$> namedType
            ]

-- | A GraphQL 'NamedType'
-- https://spec.graphql.org/draft/#NamedType
namedType :: Parser Token NamedType
namedType = traceParser "namedType" $ NamedType <$> name

-- | A GraphQL 'ListType'
-- https://spec.graphql.org/draft/#ListType
listType :: Parser Token ListType
listType = traceParser "listType" $ enclosed' '[' ']' $ ListType <$> type'

-- | A GraphQL 'NonNullType'
-- https://spec.graphql.org/draft/#NonNullType
nonNullType :: Parser Token NonNullType
nonNullType =
    traceParser "nonNullType"
        $ oneOf
            [ NonNullTypeNamed <$> namedType
            , NonNullTypeList <$> listType
            ]
        <* punctuator '!'

-- | The GraphQL 'Directives' type
-- https://spec.graphql.org/draft/#Directives
directives :: Parser Token Directives
directives = traceParser "directives" $ some1 directive

-- | A GraphQL 'Directive'
-- https://spec.graphql.org/draft/#Directive
directive :: Parser Token Directive
directive =
    traceParser "directive"
        $ Directive
        <$ punctuator '@'
        <*> name
        <*> optional arguments

-- | A GraphQL 'TypeSystemDefinition'
-- https://spec.graphql.org/draft/#TypeSystemDefinition
typeSystemDefinition :: Parser Token TypeSystemDefinition
typeSystemDefinition =
    traceParser "typeSystemDefinition"
        $ oneOf
            [ DefinitionSchema <$> schemaDefinition
            , DefinitionType <$> typeDefinition
            , DefinitionDirective <$> directiveDefinition
            ]

-- | A GraphQL 'TypeSystemExtension'
-- https://spec.graphql.org/draft/#TypeSystemExtension
typeSystemExtension :: Parser Token TypeSystemExtension
typeSystemExtension =
    traceParser "typeSystemExtension"
        $ oneOf
            [ ExtensionSchema <$> schemaExtension
            , ExtensionType <$> typeExtension
            ]

-- | A GraphQL 'SchemaDefinition'
-- https://spec.graphql.org/draft/#SchemaDefinition
schemaDefinition :: Parser Token SchemaDefinition
schemaDefinition =
    traceParser "schemaDefinition"
        $ SchemaDefinition
        <$> optional description
        <* keyword "schema"
        <*> optional directives
        <*> rootOperationTypeDefinitions

-- | A GraphQL 'SchemaExtension'
-- https://spec.graphql.org/draft/#SchemaExtension
schemaExtension :: Parser Token SchemaExtension
schemaExtension =
    traceParser "schemaExtension"
        $ SchemaExtension
        <$ keyword "extend"
        <* keyword "schema"
        <*> optional directives
        <*> ( (Just <$> rootOperationTypeDefinitions)
                <|> lookaheadCantBe' '{' Nothing
            )

-- | List of 'RootOperationTypeDefinition'
rootOperationTypeDefinitions :: Parser Token RootOperationTypeDefinitions
rootOperationTypeDefinitions =
    traceParser "rootOperationTypeDefinitions"
        $ enclosed' '{' '}'
        $ some1UniqueOn rootOperationType rootOperationTypeDefinition

-- | https://spec.graphql.org/draft/#RootOperationTypeDefinition
rootOperationTypeDefinition :: Parser Token RootOperationTypeDefinition
rootOperationTypeDefinition =
    traceParser "rootOperationTypeDefinition"
        $ RootOperationTypeDefinition
        <$> operationType
        <* punctuator ':'
        <*> namedType

-- | A GraphQL 'Description'
-- https://spec.graphql.org/draft/#Description
description :: Parser Token Description
description = traceParser "description" $ do
    TokenString stringValue <- nextToken
    pure $ Description stringValue

-- | A GraphQL 'TypeDefinition'
-- https://spec.graphql.org/draft/#TypeDefinition
typeDefinition :: Parser Token TypeDefinition
typeDefinition =
    traceParser "typeDefinition"
        $ oneOf
            [ DefinitionScalarType <$> scalarTypeDefinition
            , DefinitionObjectType <$> objectTypeDefinition
            , DefinitionInterfaceType <$> interfaceTypeDefinition
            , DefinitionUnionType <$> unionTypeDefinition
            , DefinitionEnumType <$> enumTypeDefinition
            , DefinitionInputObjectType <$> inputObjectTypeDefinition
            ]

-- | A GraphQL 'TypeExtension'
-- https://spec.graphql.org/draft/#TypeExtension
typeExtension :: Parser Token TypeExtension
typeExtension =
    traceParser "typeExtension"
        $ oneOf
            [ ExtensionScalarType <$> scalarTypeExtension
            , ExtensionObjectType <$> objectTypeExtension
            , ExtensionInterfaceType <$> interfaceTypeExtension
            , ExtensionUnionType <$> unionTypeExtension
            , ExtensionEnumType <$> enumTypeExtension
            , ExtensionInputObjectType <$> inputObjectTypeExtension
            ]

-- | A GraphQL 'ScalarTypeDefinition'
-- https://spec.graphql.org/draft/#ScalarTypeDefinition
scalarTypeDefinition :: Parser Token ScalarTypeDefinition
scalarTypeDefinition =
    traceParser "scalarTypeDefinition"
        $ ScalarTypeDefinition
        <$> optional description
        <* keyword "scalar"
        <*> name
        <*> optional directives

-- | A GraphQL 'ScalarTypeExtension'
-- https://spec.graphql.org/draft/#ScalarTypeExtension
scalarTypeExtension :: Parser Token ScalarTypeExtension
scalarTypeExtension =
    traceParser "scalarTypeExtension"
        $ ScalarTypeExtension
        <$ keyword "extend"
        <* keyword "scalar"
        <*> name
        <*> optional directives

-- | A GraphQL 'ObjectTypeDefinition'
-- https://spec.graphql.org/draft/#ObjectTypeDefinition
objectTypeDefinition :: Parser Token ObjectTypeDefinition
objectTypeDefinition =
    traceParser "objectTypeDefinition"
        $ ObjectTypeDefinition
        <$> optional description
        <* keyword "type"
        <*> name
        <*> optional implementsInterfaces
        <*> optional directives
        <*> ((Just <$> fieldsDefinition) <|> lookaheadCantBe' '{' Nothing)

-- | A GraphQL 'ObjectTypeExtension'
-- https://spec.graphql.org/draft/#ObjectTypeExtension
objectTypeExtension :: Parser Token ObjectTypeExtension
objectTypeExtension = traceParser "objectTypeExtension" $ do
    keyword "extend"
    keyword "type"
    name <- name
    implementsInterfaces <- optional implementsInterfaces
    directives <- optional directives
    fieldsDefinition <- optional fieldsDefinition
    let ok = ObjectTypeExtension name implementsInterfaces directives fieldsDefinition
    case (implementsInterfaces, directives, fieldsDefinition) of
        (Nothing, Nothing, Nothing) -> empty
        (_, _, Just{}) -> pure ok
        _ -> lookaheadCantBe' '{' ok

-- | A GraphQL 'ImplementsInterfaces'
-- https://spec.graphql.org/draft/#ImplementsInterfaces
implementsInterfaces :: Parser Token ImplementsInterfaces
implementsInterfaces =
    traceParser "implementsInterfaces"
        $ ImplementsInterfaces
        <$ keyword "implements"
        <* optional (punctuator '&')
        <*> sepBy1' '&' namedType

-- | A GraphQL 'FieldsDefinition'
-- https://spec.graphql.org/draft/#FieldsDefinitionn
fieldsDefinition :: Parser Token FieldsDefinition
fieldsDefinition =
    traceParser "fieldsDefinition"
        $ enclosed' '{' '}'
        $ FieldsDefinition
        <$> some1UniqueOn fieldDefinitionName fieldDefinition

-- | A GraphQL 'FieldDefinition'
-- https://spec.graphql.org/draft/#FieldDefinition
fieldDefinition :: Parser Token FieldDefinition
fieldDefinition =
    traceParser "fieldDefinition"
        $ FieldDefinition
        <$> optional description
        <*> name
        <*> optional argumentsDefinition
        <* punctuator ':'
        <*> type'
        <*> optional directives

-- | https://spec.graphql.org/draft/#ArgumentsDefinition
argumentsDefinition :: Parser Token ArgumentsDefinition
argumentsDefinition =
    traceParser "argumentsDefinition"
        $ enclosed' '(' ')'
        $ ArgumentsDefinition
        <$> some1UniqueOn inputValueDefinitionName inputValueDefinition

-- | https://spec.graphql.org/draft/#InputValueDefinition
inputValueDefinition :: Parser Token InputValueDefinition
inputValueDefinition =
    traceParser "inputValueDefinition"
        $ InputValueDefinition
        <$> optional description
        <*> name
        <* punctuator ':'
        <*> type'
        <*> optional defaultValue
        <*> optional directives

-- | https://spec.graphql.org/draft/#InterfaceTypeDefinition
interfaceTypeDefinition :: Parser Token InterfaceTypeDefinition
interfaceTypeDefinition =
    traceParser "interfaceTypeDefinition"
        $ InterfaceTypeDefinition
        <$> optional description
        <* keyword "interface"
        <*> name
        <*> optional implementsInterfaces
        <*> optional directives
        <*> (Just <$> fieldsDefinition <|> lookaheadCantBe' '{' Nothing)

-- | https://spec.graphql.org/draft/#InterfaceTypeExtension
interfaceTypeExtension :: Parser Token InterfaceTypeExtension
interfaceTypeExtension = traceParser "interfaceTypeExtension" $ do
    keyword "extend"
    keyword "interface"
    name <- name
    implementsInterfaces <- optional implementsInterfaces
    directives <- optional directives
    fieldsDefinition <- optional fieldsDefinition
    let ok = InterfaceTypeExtension name implementsInterfaces directives fieldsDefinition
    case (implementsInterfaces, directives, fieldsDefinition) of
        (Nothing, Nothing, Nothing) -> empty
        (_, _, Just{}) -> pure ok
        _ -> lookaheadCantBe' '{' ok

-- | https://spec.graphql.org/draft/#UnionTypeDefinition
unionTypeDefinition :: Parser Token UnionTypeDefinition
unionTypeDefinition =
    traceParser "unionTypeDefinition"
        $ UnionTypeDefinition
        <$> optional description
        <* keyword "union"
        <*> name
        <*> optional directives
        <*> optional unionMemberTypes

-- | https://spec.graphql.org/draft/#UnionMemberTypes
unionMemberTypes :: Parser Token UnionMemberTypes
unionMemberTypes =
    traceParser "unionMemberTypes"
        $ UnionMemberTypes
        <$ optional (punctuator '|')
        <*> sepBy1' '|' namedType

-- | https://spec.graphql.org/draft/#UnionTypeExtension
unionTypeExtension :: Parser Token UnionTypeExtension
unionTypeExtension = traceParser "unionTypeExtension" $ do
    keyword "extend"
    keyword "union"
    name <- name
    directives <- optional directives
    unionMemberTypes <- optional unionMemberTypes
    case (directives, unionMemberTypes) of
        (Nothing, Nothing) -> empty
        _ -> pure $ UnionTypeExtension name directives unionMemberTypes

-- | https://spec.graphql.org/draft/#EnumTypeDefinition
enumTypeDefinition :: Parser Token EnumTypeDefinition
enumTypeDefinition =
    traceParser "enumTypeDefinition"
        $ EnumTypeDefinition
        <$> optional description
        <* keyword "enum"
        <*> name
        <*> optional directives
        <*> (Just <$> enumValuesDefinition <|> lookaheadCantBe' '{' Nothing)

-- | https://spec.graphql.org/draft/#EnumValuesDefinition
enumValuesDefinition :: Parser Token EnumValuesDefinition
enumValuesDefinition =
    traceParser "enumValuesDefinition"
        $ enclosed' '{' '}'
        $ EnumValuesDefinition
        <$> some1UniqueOn enumValueDefinitionName enumValueDefinition

-- | https://spec.graphql.org/draft/#EnumValueDefinition
enumValueDefinition :: Parser Token EnumValueDefinition
enumValueDefinition =
    traceParser "enumValueDefinition"
        $ EnumValueDefinition
        <$> optional description
        <*> enumValue
        <*> optional directives

-- | https://spec.graphql.org/draft/#EnumTypeExtension
enumTypeExtension :: Parser Token EnumTypeExtension
enumTypeExtension =
    traceParser "enumTypeExtension"
        $ EnumTypeExtension
        <$ keyword "extend"
        <* keyword "enum"
        <*> name
        <*> optional directives
        <*> ((Just <$> enumValuesDefinition) <|> lookaheadCantBe' '{' Nothing)

-- | InputObjectTypeDefinition
-- https://spec.graphql.org/draft/#InputObjectTypeDefinition
inputObjectTypeDefinition :: Parser Token InputObjectTypeDefinition
inputObjectTypeDefinition =
    traceParser "inputObjectTypeDefinition"
        $ InputObjectTypeDefinition
        <$> optional description
        <* keyword "input"
        <*> name
        <*> optional directives
        <*> (Just <$> inputFieldsDefinition <|> lookaheadCantBe' '{' Nothing)

-- | InputFieldsDefinition
-- https://spec.graphql.org/draft/#InputFieldsDefinition
inputFieldsDefinition :: Parser Token InputFieldsDefinition
inputFieldsDefinition =
    traceParser "inputFieldsDefinition"
        $ enclosed' '{' '}'
        $ InputFieldsDefinition
        <$> some1UniqueOn inputValueDefinitionName inputValueDefinition

-- | InputObjectTypeExtension
-- https://spec.graphql.org/draft/#InputObjectTypeExtension
inputObjectTypeExtension :: Parser Token InputObjectTypeExtension
inputObjectTypeExtension =
    traceParser "inputObjectTypeExtension"
        $ InputObjectTypeExtension
        <$ keyword "extend"
        <* keyword "input"
        <*> name
        <*> optional directives
        <*> (Just <$> inputFieldsDefinition <|> lookaheadCantBe' '{' Nothing)

-- | Directive definition
-- https://spec.graphql.org/draft/#DirectiveDefinition
directiveDefinition :: Parser Token DirectiveDefinition
directiveDefinition =
    traceParser "directiveDefinition"
        $ DirectiveDefinition
        <$> optional description
        <* keyword "directive"
        <* punctuator '@'
        <*> name
        <*> optional argumentsDefinition
        <*> (True <$ keyword "repeatable" <|> pure False)
        <* keyword "on"
        <*> directiveLocations

-- | https://spec.graphql.org/draft/#DirectiveLocations
directiveLocations :: Parser Token DirectiveLocations
directiveLocations =
    traceParser "directiveLocations"
        $ optional (punctuator '|')
        *> sepBy1' '|' directiveLocation

-- | https://spec.graphql.org/draft/#DirectiveLocation
directiveLocation :: Parser Token DirectiveLocation
directiveLocation =
    traceParser "directiveLocation"
        $ oneOf
            [ LocationExecutableDirective <$> executableDirectiveLocation
            , LocationTypeSystemDirective <$> typeSystemDirectiveLocation
            ]

-- | https://spec.graphql.org/draft/#ExecutableDirectiveLocation
executableDirectiveLocation :: Parser Token ExecutableDirectiveLocation
executableDirectiveLocation =
    traceParser "executableDirectiveLocation"
        $ oneOf
            [ QUERY <$ keyword "QUERY"
            , MUTATION <$ keyword "MUTATION"
            , SUBSCRIPTION <$ keyword "SUBSCRIPTION"
            , FIELD <$ keyword "FIELD"
            , FRAGMENT_DEFINITION <$ keyword "FRAGMENT_DEFINITION"
            , FRAGMENT_SPREAD <$ keyword "FRAGMENT_SPREAD"
            , INLINE_FRAGMENT <$ keyword "INLINE_FRAGMENT"
            , VARIABLE_DEFINITION <$ keyword "VARIABLE_DEFINITION"
            ]

-- | https://spec.graphql.org/draft/#TypeSystemDirectiveLocation
typeSystemDirectiveLocation :: Parser Token TypeSystemDirectiveLocation
typeSystemDirectiveLocation =
    traceParser "typeSystemDirectiveLocation"
        $ oneOf
            [ SCHEMA <$ keyword "SCHEMA"
            , SCALAR <$ keyword "SCALAR"
            , OBJECT <$ keyword "OBJECT"
            , FIELD_DEFINITION <$ keyword "FIELD_DEFINITION"
            , ARGUMENT_DEFINITION <$ keyword "ARGUMENT_DEFINITION"
            , INTERFACE <$ keyword "INTERFACE"
            , UNION <$ keyword "UNION"
            , ENUM <$ keyword "ENUM"
            , ENUM_VALUE <$ keyword "ENUM_VALUE"
            , INPUT_OBJECT <$ keyword "INPUT_OBJECT"
            , INPUT_FIELD_DEFINITION <$ keyword "INPUT_FIELD_DEFINITION"
            ]

-- | A GraphQL 'Name'
-- https://spec.graphql.org/draft/#Name
name :: Parser Token Name
name = traceParser "name" $ do
    TokenName name <- nextToken
    pure name

nameButNot :: [MisoString] -> Parser Token Name
nameButNot badNames = do
    Name name <- name
    guard $ notElem name badNames
    pure $ Name name
