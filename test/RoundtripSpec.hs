module RoundtripSpec where

import Arbitrary ()
import Miso.GraphQL.Lexer (Token)
import Miso.GraphQL.Lexer qualified as Lexer
import Miso.GraphQL.Parser qualified as Parser
import Miso.GraphQL.Printer ()
import Miso.Prelude hiding (unlines)
import Miso.String (ToMisoString)
import Miso.Util.Parser (Parser, endOfInput)
import Test.Hspec
import Test.QuickCheck

roundtrip
    :: (Eq a, Show a, ToMisoString a)
    => Parser Token a
    -> a
    -> Expectation
roundtrip parser a = parsed `shouldBe` Right a
  where
    str = toMisoString a
    parsed = Parser.parse' Lexer.tokens (parser <* endOfInput) str

spec :: Spec
spec = do
    it "Name" . property $ roundtrip Parser.name
    it "Description" . property $ roundtrip Parser.description
    it "Value" . property $ roundtrip Parser.value
    it "Document" . property $ roundtrip Parser.document
