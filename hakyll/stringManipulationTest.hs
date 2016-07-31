module Main where

import Test.Hspec
import StringManipulation

main :: IO ()
main = hspec $ do
  describe "foo" $ do
    it "returns the current index if the remaining string starts with the remaining chars" $ do
       foo "abcde" 'x' "abc" 7 `shouldBe` (Just 7)

