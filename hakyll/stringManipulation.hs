module StringManipulation where

import           Data.List (elemIndex, isPrefixOf)


getExcerpt :: String -> String
getExcerpt x = untilCount x $ substringPos x "tests" 0

untilCount :: String -> Maybe Int -> String
untilCount _ Nothing = ""
untilCount string (Just i) = take i string

substringPos :: String -> String -> Int -> Maybe Int
substringPos _ [] _ = Nothing
substringPos [] _ _ = Nothing
substringPos string (c:cs) cutoff = bla (elemIndex c string) string c cs cutoff

bla :: Maybe Int -> String -> Char -> String -> Int -> Maybe Int
bla Nothing _ _ _ _ = Nothing
bla (Just i) string firstChar remainingChars cutoff = foo (drop (i+1) string) firstChar remainingChars (i+cutoff+1)

foo :: String -> Char -> String -> Int -> Maybe Int
foo remainingString firstChar remainingChars currentIndex
    | isPrefixOf remainingChars remainingString = Just currentIndex
    | otherwise = substringPos remainingString (firstChar:remainingChars) currentIndex

-- drop count list
-- elemIndex elem list
-- isPrefixOf 
