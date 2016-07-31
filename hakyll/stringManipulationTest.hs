module Main where

import Test.Hspec
import StringManipulation

main :: IO ()
main = hspec $ do
  describe "foo" $ do
    it "returns the current index if the remaining string starts with the remaining chars" $ do
       foo "abcde" "abc" 7 `shouldBe` (Just 8)

    it "adds the index into the remaining string if it contains the remaining chars" $ do
       foo "abcde" "d" 7 `shouldBe` (Just 11)

    it "returns Nothing if the remaining string does not contain remaining chars" $ do
       foo "abcde" "xyz" 7 `shouldBe` Nothing

    it "returns 0 when the substring matches from the start" $ do
      foo "abc" "bc" 0 `shouldBe` (Just 2)

    it "adds the cutoff to the result" $ do
      foo "abc" "abc" 12 `shouldBe` (Just 13)

    it "returns 6 when the substring matches from position 5" $ do
      foo "12345abc" "abc" 0 `shouldBe` (Just 6)

  describe "substringPos" $ do
    it "returns Nothing when the substring is empty" $ do
      substringPos "abcde" "" 0 `shouldBe` Nothing

    it "returns Nothing when the string to search in is empty" $ do
      substringPos "" "abc" 0 `shouldBe` Nothing

    it "returns the position of the first occurrence of the first char when the search string fully matches at that position (start of string)" $ do
      substringPos "abcde" "abc" 0 `shouldBe` (Just 1)

    it "adds the cutoff to the position" $ do
      substringPos "abcde" "abc" 15 `shouldBe` (Just 16)

    it "returns the position of the first occurrence of the first char when the search string fully matches at that position (middle of string)" $ do
      substringPos "abcdef" "cde" 0 `shouldBe` (Just 3)

    it "returns the position of the first occurrence of the first char when the search string fully matches at that position (end of string)" $ do
      substringPos "abcdef" "ef" 0 `shouldBe` (Just 5)

    it "returns Nothing when the search string is not contained in the string" $ do
      substringPos "abcde" "fg" 0 `shouldBe` Nothing

    it "returns Nothing when the search string is not fully contained in the string (start)" $ do
      substringPos "abcde" "abx" 0 `shouldBe` Nothing

    it "returns Nothing when the search string is not fully contained in the string (middle)" $ do
      substringPos "abcde" "bcx" 0 `shouldBe` Nothing

    it "returns Nothing when the search string is not fully contained in the string (end)" $ do
      substringPos "abcde" "dex" 0 `shouldBe` Nothing

    it "returns the correct position when the search string is partially and fully contained in the string" $ do
      substringPos "abcabcde" "abcd" 0 `shouldBe` (Just 4)

    it "returns the correct position when the search string is partially (multiply) and fully contained in the string" $ do
      substringPos "abcabaabcde" "abcd" 0 `shouldBe` (Just 7)

