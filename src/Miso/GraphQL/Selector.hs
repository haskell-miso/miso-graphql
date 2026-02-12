module Miso.GraphQL.Selector (module Miso.GraphQL.Selector, module Data.Row) where

import Control.Applicative (Alternative (..))
import Control.Monad ((<=<))
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.List.NonEmpty (nonEmpty)
import Data.Proxy (Proxy (..))
import Data.Row hiding (empty)
import Data.Row qualified as Row
import Data.Row.Records (eraseWithLabels)
import Data.String (IsString (fromString))
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits
import Miso.GraphQL.AST qualified as AST
import Miso.GraphQL.Class (ToGraphQL (..))
import Miso.GraphQL.TH
    ( ImplementsInterface
    , IsRootOperationType (..)
    , NamedType (..)
    )
import Miso.JSON (FromJSON, withObject, (.:))
import Miso.JSON qualified as JSON
import Miso.Prelude hiding (select)
import Miso.String (ToMisoString)

data SelectorProxy (fieldName :: Symbol) = SelectorProxy

type family FieldType (r :: Row Type) a where
    FieldType Empty a = a
    FieldType r a = Rec r -> a

data Selector t a where
    Pure :: a -> Selector t a
    Map :: (a -> b) -> Selector t a -> Selector t b
    Ap :: Selector t (a -> b) -> Selector t a -> Selector t b
    Field
        :: ( KnownSymbol fieldName
           , HasField fieldName t (FieldType r a)
           , Forall r ToGraphQL
           , FromJSON a
           )
        => SelectorProxy fieldName
        -> Rec r
        -> Selector t a
    Select
        :: ( KnownSymbol fieldName
           , HasField fieldName s (FieldType r t)
           , Forall r ToGraphQL
           , FromJSON a
           )
        => SelectorProxy fieldName
        -> Rec r
        -> Selector t a
        -> Selector s a
    SelectEach
        :: ( KnownSymbol fieldName
           , Traversable f
           , HasField fieldName s (FieldType r (f t))
           , Forall r ToGraphQL
           , FromJSON (f JSON.Value)
           )
        => SelectorProxy fieldName
        -> Rec r
        -> Selector t a
        -> Selector s (f a)
    As
        :: ( ImplementsInterface s t
           , KnownSymbol (TypeName t)
           , FromJSON a
           )
        => Proxy t
        -> Selector t a
        -> Selector s a
    Empty :: Selector t a
    Alt :: Selector t a -> Selector t a -> Selector t a

instance Functor (Selector t) where
    fmap = Map

instance Applicative (Selector t) where
    pure = Pure
    (<*>) = Ap

instance Alternative (Selector t) where
    empty = Empty
    (<|>) = Alt

instance Monoid a => Monoid (Selector t a) where
    mempty = pure mempty

instance Semigroup a => Semigroup (Selector t a) where
    (<>) = liftA2 (<>)

instance (name ~ fieldName) => IsLabel name (SelectorProxy fieldName) where
    fromLabel = SelectorProxy

instance (KnownSymbol fieldName) => ToMisoString (SelectorProxy fieldName) where
    toMisoString = fromString . symbolVal

field
    :: ( KnownSymbol fieldName
       , HasField fieldName t (FieldType r a)
       , Forall r ToGraphQL
       , FromJSON a
       )
    => SelectorProxy fieldName
    -> Rec r
    -> Selector t a
field = Field

select
    :: ( KnownSymbol fieldName
       , HasField fieldName s (FieldType r t)
       , Forall r ToGraphQL
       , FromJSON a
       )
    => SelectorProxy fieldName
    -> Rec r
    -> Selector t a
    -> Selector s a
select = Select

selectEach
    :: ( KnownSymbol fieldName
       , Traversable f
       , HasField fieldName s (FieldType r (f t))
       , Forall r ToGraphQL
       , FromJSON (f JSON.Value)
       )
    => SelectorProxy fieldName
    -> Rec r
    -> Selector t a
    -> Selector s (f a)
selectEach = SelectEach

field'
    :: ( KnownSymbol fieldName
       , HasField fieldName t (FieldType Empty a)
       , FromJSON a
       )
    => SelectorProxy fieldName
    -> Selector t a
field' = flip field Row.empty

select'
    :: ( KnownSymbol fieldName
       , HasField fieldName s (FieldType Empty t)
       , FromJSON a
       )
    => SelectorProxy fieldName
    -> Selector t a
    -> Selector s a
select' = flip select Row.empty

selectEach'
    :: ( KnownSymbol fieldName
       , Traversable f
       , HasField fieldName s (FieldType Empty (f t))
       , FromJSON (f JSON.Value)
       )
    => SelectorProxy fieldName
    -> Selector t a
    -> Selector s (f a)
selectEach' = flip selectEach Row.empty

as
    :: forall t s a
     . ( ImplementsInterface s t
       , KnownSymbol (TypeName t)
       , FromJSON a
       )
    => Selector t a
    -> Selector s a
as = As Proxy

toArguments :: (Forall r ToGraphQL) => Rec r -> Maybe AST.Arguments
toArguments =
    nonEmpty
        . fmap (uncurry AST.Argument)
        . eraseWithLabels @ToGraphQL toGraphQL

toSelectionSet :: forall t a. Selector t a -> Maybe AST.SelectionSet
toSelectionSet (Pure _) = Nothing
toSelectionSet (Map _ inner) = toSelectionSet inner
toSelectionSet (Ap selectorF selectorA) = toSelectionSet selectorF <> toSelectionSet selectorA
toSelectionSet (Field proxy args) =
    pure
        . pure
        . AST.SelectionField
        $ AST.Field
            Nothing
            (fromString . symbolVal $ proxy)
            (toArguments args)
            Nothing
            Nothing
toSelectionSet (Select proxy args inner) =
    pure
        . pure
        . AST.SelectionField
        $ AST.Field
            Nothing
            (fromString . symbolVal $ proxy)
            (toArguments args)
            Nothing
            (toSelectionSet inner)
toSelectionSet (SelectEach proxy args inner) =
    pure
        . pure
        . AST.SelectionField
        $ AST.Field
            Nothing
            (fromString . symbolVal $ proxy)
            (toArguments args)
            Nothing
            (toSelectionSet inner)
toSelectionSet (As proxy inner) =
    toSelectionSet inner
        <&> pure
            . AST.SelectionInlineFragment
            . AST.InlineFragment
                ( Just
                    . AST.TypeCondition
                    . AST.NamedType
                    . fromString
                    . symbolVal
                    . asName
                    $ proxy
                )
                Nothing
  where
    asName :: forall t. Proxy t -> Proxy (TypeName t)
    asName _ = Proxy
toSelectionSet Empty = Nothing
toSelectionSet (Alt a b) = toSelectionSet a <> toSelectionSet b

toOperationDefinition
    :: forall a b
     . (IsRootOperationType a)
    => Selector a b
    -> Maybe AST.OperationDefinition
toOperationDefinition selector =
    AST.OperationDefinition
        Nothing
        (operationType @a)
        Nothing
        Nothing
        Nothing
        <$> toSelectionSet selector

toDocument
    :: forall a b
     . (IsRootOperationType a)
    => Selector a b
    -> Maybe AST.Document
toDocument =
    fmap
        ( AST.Document
            . pure
            . AST.DefinitionExecutable
            . AST.DefinitionOperation
        )
        . toOperationDefinition

selectJSON :: Selector t a -> JSON.Value -> JSON.Parser a
selectJSON (Pure x) = const $ pure x
selectJSON (Map f s) = fmap f . selectJSON s
selectJSON (Ap a b) = \v -> selectJSON a v <*> selectJSON b v
selectJSON (Field (toMisoString -> name) _) = withObject name (.: name)
selectJSON (Select (toMisoString -> name) _ s) = withObject name (selectJSON s <=< (.: name))
selectJSON (SelectEach (toMisoString -> name) _ s) = withObject name (mapM (selectJSON s) <=< (.: name))
selectJSON (As _ s) = selectJSON s
selectJSON Empty = const empty
selectJSON (Alt a b) = \v -> selectJSON a v <|> selectJSON b v
