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
                     Just i  -> foo (drop i string) (c:cs) (cutoff+i)

foo :: String -> String -> Int -> Maybe Int
foo string searchString currentIndex
    | isPrefixOf searchString string = Just (currentIndex + 1)
    | otherwise = substringPos (drop 1 string) searchString (currentIndex + 1)
