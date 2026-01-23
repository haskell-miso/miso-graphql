{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-term-variable-capture #-}

module Miso.GraphQL.JSON where

import Control.Monad ((<=<))
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic)
import Miso.GraphQL.AST hiding (Value)
import Miso.GraphQL.Printer ()
import Miso.JSON
import Miso.Prelude hiding (Object)

newtype Request = Request {query :: MisoString}
    deriving stock (Generic)
    deriving anyclass (ToJSON)

class Operation op where
    type ReturnType op
    toOperation :: op -> OperationDefinition

execute
    :: forall op error parent model action
     . ( Operation op
       , FromJSON (ReturnType op)
       , FromJSVal error
       )
    => op
    -> MisoString
    -- ^ URL
    -> [(MisoString, MisoString)]
    -- ^ Headers
    -> (Response (Result (ReturnType op)) -> action)
    -- ^ successful callback
    -> (Response error -> action)
    -- ^ errorful callback
    -> Effect parent model action
execute op url headers successful =
    postJSON' url Request{query = toMisoString operation} headers \Response{..} ->
        successful
            Response{body = either Error Success $ parseEither executionResult body, ..}
  where
    operation = toOperation op
    (selection :| _) = operationSelectionSet operation
    executionResult :: Value -> Parser (ReturnType op)
    executionResult = withObject "ExecutionResult" $ data_ <=< (.: "data")
    data_ :: Value -> Parser (ReturnType op)
    data_ =
        case selection of
            SelectionField (Field _ (Name name) _ _ _) -> withObject "Data" $ parseJSON <=< (.: name)
            _ -> parseJSON
