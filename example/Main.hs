{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad (join)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Language.Haskell.TH.Syntax (makeRelativeToProject)
import Miso.GraphQL.Selector
import Miso.GraphQL.TH
import Miso.Prelude hiding (select)

documentFile =<< makeRelativeToProject "example/schema.gql"

peopleInSharedGroups :: Int -> Selector Query (IntMap MisoString)
peopleInSharedGroups personId =
    fmap (IntMap.fromList . maybe [] join)
        . selectEach #person (#id .== toMisoString personId)
        . selectEach' #groups
        . selectEach' #people
        $ do
            id' <- field' #id
            name <- field' #name
            pure (fromMisoString @Int id', name)

main :: IO ()
main =
    mapM_
        (putStrLn . fromMisoString . toMisoString)
        (toDocument $ peopleInSharedGroups 17)
