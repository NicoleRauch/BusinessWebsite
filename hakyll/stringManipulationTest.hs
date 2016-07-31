module Main where

import Test.Hspec
import StringManipulation

main :: IO ()
main = hspec $ do
  describe "foo" $ do
    it "returns the current index if the remaining string starts with the remaining chars" $ do
       foo "abcde" 'x' "abc" 7 `shouldBe` (Just 7)

  describe "bla" $ do
    it "returns nothing when there is no current index" $ do
      bla Nothing "abc" 'a' "bc" `shouldBe` Nothing

    it "returns 0 when the substring matches from the start" $ do
      bla (Just 0) "abc" 'a' "bc" `shouldBe` (Just 0)

    it "returns 5 when the substring matches from position 5" $ do
      bla (Just 5) "12345abc" 'a' "bc" `shouldBe` (Just 5)

  describe "substringPos" $ do
    it "returns Nothing when the substring is empty" $ do
      substringPos "abcde" "" `shouldBe` Nothing

    it "returns Nothing when the string to search in is empty" $ do
      substringPos "" "abc" `shouldBe` Nothing

    it "returns the position of the first occurrence of the first char when the search string fully matches at that position (start of string)" $ do
      substringPos "abcde" "abc" `shouldBe` (Just 0)

    it "returns the position of the first occurrence of the first char when the search string fully matches at that position (middle of string)" $ do
      substringPos "abcdef" "cde" `shouldBe` (Just 2)

    it "returns the position of the first occurrence of the first char when the search string fully matches at that position (end of string)" $ do
      substringPos "abcdef" "ef" `shouldBe` (Just 4)

    it "returns Nothing when the search string is not contained in the string" $ do
      substringPos "abcde" "fg" `shouldBe` Nothing

    it "returns Nothing when the search string is not fully contained in the string (start)" $ do
      substringPos "abcde" "abx" `shouldBe` Nothing

    it "returns Nothing when the search string is not fully contained in the string (middle)" $ do
      substringPos "abcde" "bcx" `shouldBe` Nothing

    it "returns Nothing when the search string is not fully contained in the string (end)" $ do
      substringPos "abcde" "dex" `shouldBe` Nothing

