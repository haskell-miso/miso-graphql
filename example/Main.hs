{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad (join)
import Data.Maybe (maybeToList)
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

peopleInSharedGroups :: Int -> Selector Query [Node]
peopleInSharedGroups personId =
    fmap (join . join . maybeToList)
        . selectEach #node (#id .== toMisoString personId)
        . as @Person
        . selectEach' #groups
        . selectEach' #members
        . as @Person
        $ selectNode

main :: IO ()
main =
    mapM_
        (putStrLn . fromMisoString . toMisoString)
        (toDocument $ peopleInSharedGroups 17)
