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


