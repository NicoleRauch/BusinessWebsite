module StringManipulation where

import           Data.List (elemIndex, isPrefixOf)


getExcerpt :: String -> String
getExcerpt x = untilCount x $ substringPos x "tests"

untilCount :: String -> Maybe Int -> String
untilCount _ Nothing = ""
untilCount string (Just i) = take i string

substringPos :: String -> String -> Maybe Int
substringPos _ [] = Nothing
substringPos [] _ = Nothing
substringPos string (c:cs) = bla (elemIndex c string) string c cs

bla :: Maybe Int -> String -> Char -> String -> Maybe Int
bla Nothing _ _ _ = Nothing
bla (Just i) string firstChar remainingChars = foo (drop (i+1) string) firstChar remainingChars i

foo :: String -> Char -> String -> Int -> Maybe Int
foo remainingString firstChar remainingChars currentIndex
    | isPrefixOf remainingChars remainingString = Just currentIndex
    | otherwise = substringPos remainingString (firstChar:remainingChars)

-- drop count list
-- elemIndex elem list
-- isPrefixOf 
