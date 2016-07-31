module StringManipulation where

import           Data.List (elemIndex, isPrefixOf)

getExcerpt :: String -> String
getExcerpt x = untilCount x $ substringPos x "tests" 0

untilCount :: String -> Maybe Int -> String
untilCount _ Nothing = ""
untilCount string (Just i) = take (i-1) string

substringPos :: String -> String -> Int -> Maybe Int
substringPos _ [] _ = Nothing
substringPos [] _ _ = Nothing
substringPos string (c:cs) cutoff = 
             let firstOccurrence = (elemIndex c string)
             in
                case firstOccurrence of
                     Nothing -> Nothing
                     Just i  -> checkPrefix (drop i string) (c:cs) (cutoff+i)

checkPrefix :: String -> String -> Int -> Maybe Int
checkPrefix string searchString cutoff
    | isPrefixOf searchString string = Just (cutoff + 1)
    | otherwise = substringPos (drop 1 string) searchString (cutoff + 1)
