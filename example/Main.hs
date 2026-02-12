{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad (join)
import Data.Maybe (catMaybes, maybeToList)
import GHC.Records (HasField)
import Language.Haskell.TH.Syntax (makeRelativeToProject)
import Miso.GraphQL.Selector
import Miso.GraphQL.TH
import Miso.Prelude hiding (select)

documentFile =<< makeRelativeToProject "example/schema.gql"

selectNode
    :: ( HasField "id" a ID
       , HasField "name" a MisoString
       )
    => Selector a Node
selectNode = do
    id <- field' #id
    name <- field' #name
    pure Node{..}

people :: Int -> Selector Query [Node]
people nodeId =
    fmap (join . maybeToList)
        . selectEach #node (#id .== toMisoString nodeId)
        $ (pure <$> person)
        <|> group
  where
    person :: Selector Node Node
    person = as @Person selectNode
    group :: Selector Node [Node]
    group = fmap catMaybes . as @Group . selectEach' #members $ optional person

main :: IO ()
main =
    mapM_
        (putStrLn . fromMisoString . toMisoString)
        (toDocument $ people 17)
