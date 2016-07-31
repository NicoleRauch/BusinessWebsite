module StringManipulation where

import           Data.List (elemIndex, isPrefixOf, isSuffixOf)

getExcerpt :: String -> String
getExcerpt post = let rawExcerpt = untilCount post $ substringPos post "\n\n" 0
               in removePrefix $ removeSuffix rawExcerpt

removePrefix :: String -> String
removePrefix string
        | isPrefixOf "<p>" string = drop 3 string
        | otherwise               = string

removeSuffix :: String -> String
removeSuffix string
        | isSuffixOf "</p>" string = take (length string - 4) string
        | otherwise                = string

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
