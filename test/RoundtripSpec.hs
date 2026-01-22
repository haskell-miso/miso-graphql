module RoundtripSpec where

import Miso.GraphQL.AST
import Miso.GraphQL.Lexer (Token)
import Miso.GraphQL.Lexer qualified as Lexer
import Miso.GraphQL.Parser qualified as Parser
import Miso.GraphQL.Printer ()
import Miso.Prelude hiding (unlines)
import Miso.String (ToMisoString)
import Miso.Util.Parser (Parser)
import Arbitrary ()
import Test.QuickCheck
import Test.Hspec

roundtrip :: (Eq a, Show a, ToMisoString a) => Parser Token a -> a -> Property
roundtrip parser a =
    counterexample (fromMisoString str)
        . counterexample ("<- " <> show lexed)
        . counterexample ("<- " <> show parsed)
        $ Right a == parsed
  where
    str = toMisoString a
    lexed = Lexer.lex Lexer.tokens str
    parsed = Parser.parse' Lexer.tokens parser str

spec :: Spec
spec = it "round trips" . property $ roundtrip @Document Parser.document
