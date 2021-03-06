module Card.Parser where

import Text.ParserCombinators.Parsec
import Card.Type (Locale(..), SoundType(..))
import Data.Maybe
import qualified Data.Set as S

data CardTag
    = Loc Locale
    | Snd SoundType
    | Golden
    | PrintText
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

parseCards :: String -> [(S.Set CardTag, String)]
parseCards str = case parse (many $ try one) "" str of
    Left _ -> []
    Right xs -> map (\(ts, n) -> (S.fromList $ mapMaybe cardTag ts, n)) xs
    where
        one = card <|> (anyChar >> one)

cardTag :: (String, String) -> Maybe CardTag
cardTag ("ru", _) = Just $ Loc (Locale "ruRU")
cardTag ("en", _) = Just $ Loc (Locale "enUS")
cardTag ("locale", l) = Just $ Loc (Locale l)
cardTag ("sound", "death") = Just $ Snd Death
cardTag ("sound", "attack") = Just $ Snd Attack
cardTag ("sound", "play") = Just $ Snd Play
cardTag ("golden", _) = Just Golden
cardTag ("gold", _) = Just Golden
cardTag ("text", _) = Just PrintText
cardTag _ = Nothing
