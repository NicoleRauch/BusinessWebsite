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
substringPos string (c:cs) cutoff = 
             let firstOccurrence = (elemIndex c string)
             in
                case firstOccurrence of
                     Nothing -> Nothing
                     Just i  -> foo (drop (i+1) string) c cs (cutoff+i+1)

foo :: String -> Char -> String -> Int -> Maybe Int
foo remainingString firstChar remainingChars currentIndex
    | isPrefixOf remainingChars remainingString = Just currentIndex
    | otherwise = substringPos remainingString (firstChar:remainingChars) currentIndex
