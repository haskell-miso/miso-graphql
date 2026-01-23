module Miso.GraphQL.Class where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import GHC.Float (float2Double)
import Miso.GraphQL.AST
import Miso.JSON (ToJSON (..))
import Miso.JSON qualified as JSON
import Miso.Prelude
import Miso.String qualified as MisoString

class ToGraphQL v where
    toGraphQL :: v -> Value
    default toGraphQL :: (ToJSON v) => v -> Value
    toGraphQL = toGraphQL . toJSON

instance ToGraphQL JSON.Value where
    toGraphQL (JSON.Number n) = toGraphQL n
    toGraphQL (JSON.Bool b) = toGraphQL b
    toGraphQL (JSON.String s) = toGraphQL s
    toGraphQL (JSON.Array xs) = toGraphQL xs
    toGraphQL (JSON.Object o) = toGraphQL o
    toGraphQL JSON.Null = ValueNull

instance (ToGraphQL v) => ToGraphQL [v] where
    toGraphQL = ValueList . fmap toGraphQL

instance (ToGraphQL v) => ToGraphQL (NonEmpty v) where
    toGraphQL = toGraphQL . NonEmpty.toList

instance (ToGraphQL v) => ToGraphQL (Maybe v) where
    toGraphQL = maybe ValueNull toGraphQL

instance (ToGraphQL v) => ToGraphQL (Map MisoString v) where
    toGraphQL =
        ValueObject . fmap (\(k, v) -> ObjectField (Name k) (toGraphQL v)) . Map.toList

instance ToGraphQL Int where
    toGraphQL = ValueInt

instance ToGraphQL Float where
    toGraphQL = ValueFloat . float2Double

instance ToGraphQL Double where
    toGraphQL = ValueFloat

instance ToGraphQL MisoString where
    toGraphQL s
        | isJust . MisoString.find (`elem` ['\r', '\n']) $ s =
            ValueString $ BlockString s
        | otherwise = ValueString $ SingleLineString s

instance ToGraphQL Bool where
    toGraphQL = ValueBoolean
