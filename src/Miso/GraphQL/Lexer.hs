module Miso.GraphQL.Lexer where

import Control.Applicative (Alternative (many, some, empty), optional, (<|>))
import Control.Monad (guard, replicateM)
import Data.Foldable (Foldable (fold, toList))
import Data.Functor (void)
import Data.Ix (Ix (inRange))
import Data.Maybe (catMaybes, isJust)
import GHC.Generics (Generic)
import Miso.GraphQL.AST
import Miso.Prelude
import Miso.String qualified as MisoString
import Miso.Util.Lexer hiding (token)
import Debug.Trace

lex :: Lexer a -> MisoString -> Either LexerError a
lex lexer = trace "OH BOY HERE WE GO LEXING" $ fmap fst . runLexer lexer . mkStream . traceShowId

-- | Token unit for lexing the GraphQL specification
-- https://spec.graphql.org/draft/#Token
data Token
    = TokenPunctuator Char
    | TokenEllipsis
    | TokenName Name
    | TokenInt Int
    | TokenFloat Double
    | TokenString StringValue
    deriving stock (Show, Eq, Generic)

concatM :: (Traversable t, Monad m, Monoid a) => t (m a) -> m a
concatM xs = fold <$> sequence xs

concatMaybeM :: (Traversable t, Monad m, Monoid a) => t (m (Maybe a)) -> m a
concatMaybeM xs = fold . catMaybes . toList <$> sequence xs

traceLexer :: (Show a) => String -> Lexer a -> Lexer a
traceLexer s l = do
    traceM . (("lexer> " <> s <> " ") <>) . show =<< withLocation peek
    a <-
        l <|> do
            traceM $ "!lexer " <> s
            empty
    traceM $ "<lexer " <> s <> ": " <> show a
    pure a

inAnyRange :: (Foldable t, Ix a) => t (a, a) -> a -> Bool
inAnyRange = flip $ any . flip inRange

-- | https://spec.graphql.org/draft/#SourceCharacter
sourceCharacter :: Lexer Char
sourceCharacter = traceLexer "sourceCharacter" $ satisfy $ const True

-- | https://spec.graphql.org/draft/#Letter
letter :: Lexer Char
letter = traceLexer "letter" $ satisfy $ inAnyRange [('A', 'Z'), ('a', 'z')]

-- | https://spec.graphql.org/draft/#Digit
digit :: Lexer Char
digit = traceLexer "digit" $ satisfy $ inRange ('0', '9')

-- | https://spec.graphql.org/draft/#HexDigit
hexDigit :: Lexer Char
hexDigit = traceLexer "hexDigit" $ satisfy $ inAnyRange [('0', '9'), ('A', 'F'), ('a', 'f')]

-- | https://spec.graphql.org/draft/#NonZeroDigit
nonZeroDigit :: Lexer Char
nonZeroDigit = traceLexer "nonZeroDigit" $ satisfy $ inRange ('1', '9')

-- | https://spec.graphql.org/draft/#IntegerPart
integerPart :: Lexer MisoString
integerPart = traceLexer "integerPart" $
    concatMaybeM
        [ optional $ string "-"
        , Just
            <$> oneOf
                [ string "0"
                , toMisoString <$> liftA2 (:) nonZeroDigit (many digit)
                ]
        ]

-- | https://spec.graphql.org/draft/#IntValue
intValue :: Lexer Int
intValue = traceLexer "intValue" $ fromMisoString <$> integerPart

-- | https://spec.graphql.org/draft/#FractionalPart
fractionalPart :: Lexer MisoString
fractionalPart = traceLexer "fractionalPart" $
    concatM
        [ string "."
        , toMisoString <$> some digit
        ]

-- | https://spec.graphql.org/draft/#ExponentIndicator
exponentIndicator :: Lexer Char
exponentIndicator = traceLexer "exponentIndicator" $ char 'e' <|> char 'E'

-- | https://spec.graphql.org/draft/#Sign
sign :: Lexer Char
sign = traceLexer "sign" $ char '+' <|> char '-'

-- | https://spec.graphql.org/draft/#ExponentPart
exponentPart :: Lexer MisoString
exponentPart = traceLexer "exponentPart" $
    concatMaybeM
        [ Just . toMisoString <$> exponentIndicator
        , fmap toMisoString <$> optional sign
        , Just . toMisoString <$> some digit
        ]

-- | https://spec.graphql.org/draft/#FloatValue
floatValue :: Lexer Double
floatValue = traceLexer "floatValue" $
    fromMisoString
        <$> concatMaybeM
            [ Just <$> integerPart
            , optional fractionalPart
            , optional exponentPart
            ]

-- | https://spec.graphql.org/draft/#Name
name :: Lexer Name
name = traceLexer "name" $
    Name
        <$> concatM
            [ toMisoString <$> (letter <|> char '_')
            , toMisoString <$> many (letter <|> digit <|> char '_')
            ]

-- | https://spec.graphql.org/draft/#StringValue
stringValue :: Lexer StringValue
stringValue = traceLexer "stringValue" $
    oneOf
        [ BlockString <$> blockString
        , SingleLineString <$> singleLineString
        ]

singleLineString :: Lexer MisoString
singleLineString = traceLexer "singleLineString" $ delimiter *> go
  where
    delimiter :: Lexer ()
    delimiter = void $ traceLexer "singleLineString delimiter" $ char '\"'
    -- https://spec.graphql.org/draft/#EscapedCharacter
    escapedCharacter :: Lexer Char
    escapedCharacter = oneOf $ char <$> "\"\\/bfnrt"
    -- https://spec.graphql.org/draft/#EscapedUnicode
    escapedUnicode :: Lexer MisoString
    escapedUnicode =
        oneOf
            [ concatM
                [ string "{"
                , toMisoString <$> some hexDigit
                , string "}"
                ]
            , toMisoString <$> replicateM 4 hexDigit
            ]
    go :: Lexer MisoString
    go =
        optional delimiter >>= \case
            Just _ -> pure ""
            Nothing ->
                oneOf
                    [ concatM [string "\\", toMisoString <$> escapedCharacter, go]
                    , concatM [string "\\u", escapedUnicode, go]
                    , liftA2 MisoString.cons nonLineTerminator go
                    ]

-- | https://spec.graphql.org/draft/#BlockString
blockString :: Lexer MisoString
blockString = traceLexer "blockString" $ delimiter *> go
  where
    delimiter :: Lexer ()
    delimiter = void $ traceLexer "blockString delimiter" $ string "\"\"\""
    go :: Lexer MisoString
    go =
        optional delimiter >>= \case
            Just _ -> pure ""
            Nothing ->
                oneOf
                    [ "" <$ delimiter
                    , concatM
                        [ toMisoString <$> char '\\'
                        , toMisoString <$> sourceCharacter
                        , go
                        ]
                    , liftA2 MisoString.cons sourceCharacter go
                    ]

-- | https://spec.graphql.org/draft/#Punctuator
punctuator :: Lexer Char
punctuator = traceLexer "punctuator" $ oneOf $ char <$> "!$&():=@[]{|}"

ellipsis :: Lexer ()
ellipsis = traceLexer "ellipsis" $ void $ string "..."

-- | https://spec.graphql.org/draft/#NullValue
-- | https://spec.graphql.org/draft/#LineTerminator
lineTerminator :: Lexer ()
lineTerminator = traceLexer "lineTerminator" $ do
    r <- optional . char $ '\r'
    n <- optional . char $ '\n'
    guard $ isJust r || isJust n

nonLineTerminator :: Lexer Char
nonLineTerminator = traceLexer "nonLineTerminator" $ satisfy $ not . (`elem` ['\r', '\n'])

-- | https://spec.graphql.org/draft/#Comment
comment :: Lexer ()
comment = void $ traceLexer "comment" $ char '#' *> many nonLineTerminator

-- | https://spec.graphql.org/draft/#Ignored
ignored :: Lexer ()
ignored = traceLexer "ignored" $
    oneOf
        [ unicodeBom
        , whitespace
        , lineTerminator
        , comment
        , comma
        ]
  where
    -- https://spec.graphql.org/draft/#UnicodeBOM
    unicodeBom = void $ traceLexer "unicodeBom" $ char '\xFEFF'
    -- https://spec.graphql.org/draft/#Whitespace
    whitespace = void $ traceLexer "whitespace" $ some $ char '\t' <|> char ' '
    -- https://spec.graphql.org/draft/#Comma
    comma = void $ traceLexer "comma" $ char ','

token :: Lexer Token
token = traceLexer "token" $
    oneOf
        [ TokenPunctuator <$> punctuator
        , TokenEllipsis <$ ellipsis
        , TokenInt <$> intValue
        , TokenFloat <$> floatValue
        , TokenString <$> stringValue
        , TokenName <$> name
        ]

tokens :: Lexer [Token]
tokens = traceLexer "tokens" $ some $ many ignored *> token
