module Miso.GraphQL.Parser where

import Control.Applicative (Alternative (empty, many, some), optional, (<|>))
import Control.Monad (guard)
import Data.Functor (void)
import Miso.GraphQL.AST
import Miso.GraphQL.Lexer (Token (..), lex)
import Miso.Prelude hiding (lex)
import Miso.Util.Parser

punctuator :: Char -> Parser Token ()
punctuator = void . token_ . TokenPunctuator

keyword :: MisoString -> Parser Token ()
keyword = void . token_ . TokenName . Name

enclosed' :: Char -> Char -> Parser Token a -> Parser Token a
enclosed' a b = enclosed (punctuator a) (punctuator b)

sepBy1' :: Char -> Parser Token a -> Parser Token [a]
sepBy1' = sepBy1 . punctuator

lookaheadCantBe :: (Eq token) => token -> a -> Parser token a
lookaheadCantBe token a = do
    guard . (token /=) =<< peek
    pure a

lookaheadCantBe' :: Char -> a -> Parser Token a
lookaheadCantBe' = lookaheadCantBe . TokenPunctuator

nextToken :: ParserT r [a] [] a
nextToken = satisfy $ const True

parseDocument :: MisoString -> Either (ParseError Document Token) Document
parseDocument = either (Left . LexicalError) (parse document) . lex

-- | https://spec.graphql.org/draft/#Document
document :: Parser Token Document
document = Document <$> some definition

-- | https://spec.graphql.org/draft/#Definition
definition :: Parser Token Definition
definition =
    oneOf
        [ DefinitionExecutable <$> executableDefinition
        , DefinitionTypeSystem <$> typeSystemDefinition
        , ExtensionTypeSystem <$> typeSystemExtension
        ]

-- https://spec.graphql.org/draft/#ExecutableDefinition
executableDefinition :: Parser Token ExecutableDefinition
executableDefinition =
    oneOf
        [ DefinitionOperation <$> operationDefinition
        , DefinitionFragment <$> fragmentDefinition
        ]

-- | A GraphQL 'OperationDefinition'
-- https://spec.graphql.org/draft/#OperationDefinition
operationDefinition :: Parser Token OperationDefinition
operationDefinition =
    oneOf
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
    oneOf
        [ Query <$ keyword "query"
        , Mutation <$ keyword "mutation"
        , Subscription <$ keyword "subscription"
        ]

-- | A GraphQL 'SelectionSet'
-- https://spec.graphql.org/draft/#SelectionSet
selectionSet :: Parser Token SelectionSet
selectionSet = enclosed' '{' '}' $ some selection

-- | A GraphQL 'Selection' type
-- https://spec.graphql.org/draft/#Selection
selection :: Parser Token Selection
selection =
    oneOf
        [ SelectionField <$> field
        , SelectionFragmentSpread <$> fragmentSpread
        , SelectionInlineFragment <$> inlineFragment
        ]

-- | A GraphQL 'Field' type
-- https://spec.graphql.org/draft/#Field
field :: Parser Token Field
field =
    Field
        <$> optional alias
        <*> name
        <*> optional arguments
        <*> optional directives
        <*> optional selectionSet

-- | A GraphQL 'Alias'
-- https://spec.graphql.org/draft/#Alias
alias :: Parser Token Alias
alias = Alias <$> name <* punctuator ':'

-- | GraphQL 'Arguments'
-- https://spec.graphql.org/draft/#Arguments
arguments :: Parser Token Arguments
arguments = enclosed' '(' ')' $ some argument

-- | A GraphQL 'Argument'
-- https://spec.graphql.org/draft/#Arguments
argument :: Parser Token Argument
argument = Argument <$> name <* punctuator ':' <*> value

-- | GraphQL 'FragmentSpread' type
-- https://spec.graphql.org/draft/#FragmentSpread
fragmentSpread :: Parser Token FragmentSpread
fragmentSpread =
    FragmentSpread
        <$ token_ TokenEllipsis
        <*> fragmentName
        <*> optional directives

-- | GraphQL 'InlineFragment' type
-- https://spec.graphql.org/draft/#InlineFragment
inlineFragment :: Parser Token InlineFragment
inlineFragment =
    InlineFragment
        <$ token_ TokenEllipsis
        <*> optional typeCondition
        <*> optional directives
        <*> selectionSet

-- | A GraphQL 'FragmentDefinition'
-- https://spec.graphql.org/draft/#FragmentDefinition
fragmentDefinition :: Parser Token FragmentDefinition
fragmentDefinition =
    FragmentDefinition
        <$> optional description
        <* keyword "fragment"
        <*> fragmentName
        <*> typeCondition
        <*> optional directives
        <*> selectionSet

-- | A GraphQL 'FragmentName'
-- https://spec.graphql.org/draft/#FragmentName
fragmentName :: Parser Token FragmentName
fragmentName = FragmentName <$> nameButNot ["on"]

-- | A GraphQL 'TypeCondition'
-- https://spec.graphql.org/draft/#TypeCondition
typeCondition :: Parser Token TypeCondition
typeCondition = TypeCondition <$ keyword "on" <*> namedType

-- | A GraphQL 'Value'
-- https://spec.graphql.org/draft/#Value
value :: Parser Token Value
value =
    oneOf
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
listValue = enclosed' '[' ']' $ many value

-- | https://spec.graphql.org/draft/#ObjectValue
objectValue :: Parser Token [ObjectField]
objectValue = enclosed' '{' '}' $ many objectField

-- | A GraphQL 'EnumValue'
-- https://spec.graphql.org/draft/#EnumValue
enumValue :: Parser Token EnumValue
enumValue = EnumValue <$> nameButNot ["true", "false", "null"]

-- | A GraphQL 'ObjectField'
-- https://spec.graphql.org/draft/#ObjectField
objectField :: Parser Token ObjectField
objectField = ObjectField <$> name <* punctuator ':' <*> value

-- | GraphQL 'VariablesDefinition'
-- https://spec.graphql.org/draft/#VariablesDefinition
variablesDefinition :: Parser Token VariablesDefinition
variablesDefinition = enclosed' '(' ')' (some variableDefinition)

-- | A GraphQL 'VariableDefinition'
-- https://spec.graphql.org/draft/#VariableDefinition
variableDefinition :: Parser Token VariableDefinition
variableDefinition =
    VariableDefinition
        <$> optional description
        <*> variable
        <* punctuator ':'
        <*> type'
        <*> optional defaultValue
        <*> optional directives

-- | A GraphQL 'Variable'
-- https://spec.graphql.org/draft/#Variable
variable :: Parser Token Variable
variable = Variable <$ punctuator '$' <*> name

-- | A GraphQL 'DefaultValue'
-- https://spec.graphql.org/draft/#DefaultValue
defaultValue :: Parser Token DefaultValue
defaultValue = DefaultValue <$> value

-- | A GraphQL 'Type'
-- https://spec.graphql.org/draft/#Type
type' :: Parser Token Type
type' =
    oneOf
        [ TypeNamed <$> namedType
        , TypeList <$> listType
        , TypeNonNull <$> nonNullType
        ]

-- | A GraphQL 'NamedType'
-- https://spec.graphql.org/draft/#NamedType
namedType :: Parser Token NamedType
namedType = NamedType <$> name

-- | A GraphQL 'ListType'
-- https://spec.graphql.org/draft/#ListType
listType :: Parser Token ListType
listType = ListType <$> enclosed' '[' ']' type'

-- | A GraphQL 'NonNullType'
-- https://spec.graphql.org/draft/#NonNullType
nonNullType :: Parser Token NonNullType
nonNullType =
    oneOf
        [ NonNullTypeNamed <$> namedType
        , NonNullTypeList <$> listType
        ]
        <* punctuator '!'

-- | The GraphQL 'Directives' type
-- https://spec.graphql.org/draft/#Directives
directives :: Parser Token Directives
directives = some directive

-- | A GraphQL 'Directive'
-- https://spec.graphql.org/draft/#Directive
directive :: Parser Token Directive
directive = Directive <$ punctuator '@' <*> name <*> optional arguments

-- | A GraphQL 'TypeSystemDefinition'
-- https://spec.graphql.org/draft/#TypeSystemDefinition
typeSystemDefinition :: Parser Token TypeSystemDefinition
typeSystemDefinition =
    oneOf
        [ DefinitionSchema <$> schemaDefinition
        , DefinitionType <$> typeDefinition
        , DefinitionDirective <$> directiveDefinition
        ]

-- | A GraphQL 'TypeSystemExtension'
-- https://spec.graphql.org/draft/#TypeSystemExtension
typeSystemExtension :: Parser Token TypeSystemExtension
typeSystemExtension =
    oneOf
        [ ExtensionSchema <$> schemaExtension
        , ExtensionType <$> typeExtension
        ]

-- | A GraphQL 'SchemaDefinition'
-- https://spec.graphql.org/draft/#SchemaDefinition
schemaDefinition :: Parser Token SchemaDefinition
schemaDefinition =
    SchemaDefinition
        <$> optional description
        <* keyword "schema"
        <*> optional directives
        <*> rootOperationTypeDefinitions

-- | A GraphQL 'SchemaExtension'
-- https://spec.graphql.org/draft/#SchemaExtension
schemaExtension :: Parser Token SchemaExtension
schemaExtension =
    SchemaExtension
        <$ keyword "extend"
        <* keyword "schema"
        <*> optional directives
        <*> ( (Just <$> rootOperationTypeDefinitions)
                <|> lookaheadCantBe' '{' Nothing
            )

-- | List of 'RootOperationTypeDefinition'
rootOperationTypeDefinitions :: Parser Token RootOperationTypeDefinitions
rootOperationTypeDefinitions = enclosed' '{' '}' $ some rootOperationTypeDefinition

-- | https://spec.graphql.org/draft/#RootOperationTypeDefinition
rootOperationTypeDefinition :: Parser Token RootOperationTypeDefinition
rootOperationTypeDefinition =
    RootOperationTypeDefinition
        <$> operationType
        <* punctuator ':'
        <*> namedType

-- | A GraphQL 'Description'
-- https://spec.graphql.org/draft/#Description
description :: Parser Token Description
description = do
    TokenString stringValue <- nextToken
    pure $ Description stringValue

-- | A GraphQL 'TypeDefinition'
-- https://spec.graphql.org/draft/#TypeDefinition
typeDefinition :: Parser Token TypeDefinition
typeDefinition =
    oneOf
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
    oneOf
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
    ScalarTypeDefinition
        <$> optional description
        <* keyword "scalar"
        <*> name
        <*> optional directives

-- | A GraphQL 'ScalarTypeExtension'
-- https://spec.graphql.org/draft/#ScalarTypeExtension
scalarTypeExtension :: Parser Token ScalarTypeExtension
scalarTypeExtension =
    ScalarTypeExtension
        <$ keyword "extend"
        <* keyword "scalar"
        <*> name
        <*> optional directives

-- | A GraphQL 'ObjectTypeDefinition'
-- https://spec.graphql.org/draft/#ObjectTypeDefinition
objectTypeDefinition :: Parser Token ObjectTypeDefinition
objectTypeDefinition =
    ObjectTypeDefinition
        <$> optional description
        <* keyword "type"
        <*> name
        <*> optional implementsInterfaces
        <*> optional directives
        <*> ((Just <$> fieldsDefinition) <|> lookaheadCantBe' '{' Nothing)

-- | A GraphQL 'ObjectTypeExtension'
-- https://spec.graphql.org/draft/#ObjectTypeExtension
objectTypeExtension :: Parser Token ObjectTypeExtension
objectTypeExtension = do
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
    ImplementsInterfaces
        <$ keyword "implements"
        <* optional (punctuator '&')
        <*> sepBy1' '&' namedType

-- | A GraphQL 'FieldsDefinition'
-- https://spec.graphql.org/draft/#FieldsDefinitionn
fieldsDefinition :: Parser Token FieldsDefinition
fieldsDefinition = enclosed' '{' '}' $ FieldsDefinition <$> some fieldDefinition

-- | A GraphQL 'FieldDefinition'
-- https://spec.graphql.org/draft/#FieldDefinition
fieldDefinition :: Parser Token FieldDefinition
fieldDefinition =
    FieldDefinition
        <$> optional description
        <*> name
        <*> optional argumentsDefinition
        <* punctuator ':'
        <*> type'
        <*> optional directives

-- | https://spec.graphql.org/draft/#ArgumentsDefinition
argumentsDefinition :: Parser Token ArgumentsDefinition
argumentsDefinition = enclosed' '(' ')' $ ArgumentsDefinition <$> some inputValueDefinition

-- | https://spec.graphql.org/draft/#InputValueDefinition
inputValueDefinition :: Parser Token InputValueDefinition
inputValueDefinition =
    InputValueDefinition
        <$> optional description
        <*> name
        <* punctuator ':'
        <*> type'
        <*> optional defaultValue
        <*> optional directives

-- | https://spec.graphql.org/draft/#InterfaceTypeDefinition
interfaceTypeDefinition :: Parser Token InterfaceTypeDefinition
interfaceTypeDefinition =
    InterfaceTypeDefinition
        <$> optional description
        <* keyword "interface"
        <*> name
        <*> optional implementsInterfaces
        <*> optional directives
        <*> (Just <$> fieldsDefinition <|> lookaheadCantBe' '{' Nothing)

-- | https://spec.graphql.org/draft/#InterfaceTypeExtension
interfaceTypeExtension :: Parser Token InterfaceTypeExtension
interfaceTypeExtension = do
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
    UnionTypeDefinition
        <$> optional description
        <* keyword "union"
        <*> name
        <*> optional directives
        <*> optional unionMemberTypes

-- | https://spec.graphql.org/draft/#UnionMemberTypes
unionMemberTypes :: Parser Token UnionMemberTypes
unionMemberTypes =
    UnionMemberTypes
        <$ optional (punctuator '|')
        <*> sepBy1' '|' namedType

-- | https://spec.graphql.org/draft/#UnionTypeExtension
unionTypeExtension :: Parser Token UnionTypeExtension
unionTypeExtension = do
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
    EnumTypeDefinition
        <$> optional description
        <* keyword "enum"
        <*> name
        <*> optional directives
        <*> (Just <$> enumValuesDefinition <|> lookaheadCantBe' '{' Nothing)

-- | https://spec.graphql.org/draft/#EnumValuesDefinition
enumValuesDefinition :: Parser Token EnumValuesDefinition
enumValuesDefinition = enclosed' '{' '}' $ EnumValuesDefinition <$> some enumValueDefinition

-- | https://spec.graphql.org/draft/#EnumValueDefinition
enumValueDefinition :: Parser Token EnumValueDefinition
enumValueDefinition =
    EnumValueDefinition
        <$> optional description
        <*> enumValue
        <*> optional directives

-- | https://spec.graphql.org/draft/#EnumTypeExtension
enumTypeExtension :: Parser Token EnumTypeExtension
enumTypeExtension =
    EnumTypeExtension
        <$ keyword "extend"
        <* keyword "enum"
        <*> name
        <*> optional directives
        <*> (Just <$> enumValuesDefinition <|> lookaheadCantBe' '{' Nothing)

-- | InputObjectTypeDefinition
-- https://spec.graphql.org/draft/#InputObjectTypeDefinition
inputObjectTypeDefinition :: Parser Token InputObjectTypeDefinition
inputObjectTypeDefinition =
    InputObjectTypeDefinition
        <$> optional description
        <* keyword "input"
        <*> name
        <*> optional directives
        <*> (Just <$> inputFieldsDefinition <|> lookaheadCantBe' '{' Nothing)

-- | InputFieldsDefinition
-- https://spec.graphql.org/draft/#InputFieldsDefinition
inputFieldsDefinition :: Parser Token InputFieldsDefinition
inputFieldsDefinition = enclosed' '{' '}' $ InputFieldsDefinition <$> some inputValueDefinition

-- | InputObjectTypeExtension
-- https://spec.graphql.org/draft/#InputObjectTypeExtension
inputObjectTypeExtension :: Parser Token InputObjectTypeExtension
inputObjectTypeExtension =
    InputObjectTypeExtension
        <$ keyword "extend"
        <* keyword "input"
        <*> name
        <*> optional directives
        <*> (Just <$> inputFieldsDefinition <|> lookaheadCantBe' '{' Nothing)

-- | Directive definition
-- https://spec.graphql.org/draft/#DirectiveDefinition
directiveDefinition :: Parser Token DirectiveDefinition
directiveDefinition =
    DirectiveDefinition
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
directiveLocations = optional (punctuator '|') *> sepBy1' '|' directiveLocation

-- | https://spec.graphql.org/draft/#DirectiveLocation
directiveLocation :: Parser Token DirectiveLocation
directiveLocation =
    oneOf
        [ LocationExecutableDirective <$> executableDirectiveLocation
        , LocationTypeSystemDirective <$> typeSystemDirectiveLocation
        ]

-- | https://spec.graphql.org/draft/#ExecutableDirectiveLocation
executableDirectiveLocation :: Parser Token ExecutableDirectiveLocation
executableDirectiveLocation =
    oneOf
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
    oneOf
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
name = do
    TokenName name <- nextToken
    pure name

nameButNot :: [MisoString] -> Parser Token Name
nameButNot badNames = do
    Name name <- name
    guard $ notElem name badNames
    pure $ Name name
