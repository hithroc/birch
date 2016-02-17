module LolEcho (lolecho) where

import Data.Char
import Data.Maybe

convert :: Eq a => [(a, b)] -> (a -> b) -> a -> b
convert table f x = fromMaybe (f x) (lookup x table)

latin :: Char -> String
latin = convert table return
    where
        table =
            [ ('а', "a")
            , ('б', "b")
            , ('в', "v")
            , ('г', "g")
            , ('д', "d")
            , ('е', "e")
            , ('ё', "yo")
            , ('ж', "zh")
            , ('з', "z")
            , ('и', "i")
            , ('й', "y")
            , ('к', "k")
            , ('л', "l")
            , ('м', "m")
            , ('н', "n")
            , ('о', "o")
            , ('п', "p")
            , ('р', "r")
            , ('с', "s")
            , ('т', "t")
            , ('у', "u")
            , ('ф', "f")
            , ('х', "h")
            , ('ц', "c")
            , ('ч', "ch")
            , ('ш', "sh")
            , ('щ', "sch")
            , ('ъ', "y")
            , ('ы', "y")
            , ('ь', "'")
            , ('э', "e")
            , ('ю', "yu")
            , ('я', "ya")
            ]

ugly :: Char -> String
ugly = convert table return
    where
        table =
            [ ('a', "а")
            , ('b', "ь")
            , ('c', "с")
            , ('d', "д")
            , ('e', "е")
            , ('f', "ф")
            , ('g', "9")
            , ('h', "н")
            , ('i', "1")
            , ('j', "ж")
            , ('k', "к")
            , ('l', "1")
            , ('m', "м")
            , ('n', "п")
            , ('o', "о")
            , ('p', "р")
            , ('q', "ч")
            , ('r', "г")
            , ('s', "5")
            , ('t', "т")
            , ('u', "и")
            , ('v', "ц")
            , ('w', "ш")
            , ('x', "х")
            , ('y', "у")
            , ('z', "2")
            ]

lolecho :: String -> String
lolecho = concatMap ugly . concatMap (latin . toLower)
