{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-term-variable-capture #-}

module Miso.GraphQL.JSON where

import Control.Monad ((<=<))
import GHC.Generics (Generic)
import Miso.GraphQL.Printer ()
import Miso.GraphQL.Selector
import Miso.GraphQL.TH
import Miso.JSON
import Miso.Prelude hiding (Object)
import Miso.String (FromMisoString, ToMisoString)

newtype Request = Request {query :: MisoString}
    deriving stock (Generic)
    deriving anyclass (ToJSON)
    deriving newtype (FromMisoString, ToMisoString)

execute
    :: forall a b error parent model action
     . (IsRootOperationType a, FromJSVal error)
    => Selector a b
    -> MisoString
    -- ^ URL
    -> [(MisoString, MisoString)]
    -- ^ Headers
    -> (Response (Result b) -> action)
    -- ^ successful callback
    -> (Response error -> action)
    -- ^ errorful callback
    -> Effect parent model action
execute selector url headers successful =
    postJSON' url request headers \Response{..} ->
        successful
            Response
                { body = either Error Success $ parseEither executionResult body
                , ..
                }
  where
    request = Request . maybe "" toMisoString . toDocument $ selector
    executionResult :: Value -> Parser b
    executionResult = withObject "ExecutionResult" $ selectJSON selector <=< (.: "data")
