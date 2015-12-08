module Card.Parser where

import Text.ParserCombinators.Parsec
import Data.Maybe
import qualified Data.Set as S

import Debug.Trace

data CardTag = Locale String
    deriving (Show, Eq, Ord)

tag :: Parser (String, String)
tag = do
    k <- many1 alphaNum
    v <- option "" $ do
        _ <- char ':'
        many1 alphaNum
    _ <- char '#'
    return (k, v)

card :: Parser ([(String, String)], String)
card = do
    char '['
    char '['
    tags <- many (try tag)
    name <- many1 $ try $ do
        notFollowedBy (char ']' *> char ']')
        anyChar
    char ']'
    char ']'
    return (tags, name)

getCards :: String -> [(S.Set CardTag, String)]
getCards str = case parse (many $ try one) "" str of
    Left e -> traceShow e []
    Right xs -> map (\(ts, n) -> (S.fromList $ mapMaybe cardTag ts, n)) xs
    where
        one = card <|> (anyChar >> one)

cardTag :: (String, String) -> Maybe CardTag
cardTag ("ru", _) = Just $ Locale "ruRU"
cardTag ("en", _) = Just $ Locale "enUS"
cardTag ("locale", l) = Just $ Locale l
cardTag _ = Nothing
