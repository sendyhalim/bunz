{-# LANGUAGE OverloadedStrings #-}

module BeautifierSpec (main, spec) where

import qualified Beautifier  as B
import           Data.Monoid ((<>))
import           Test.Hspec

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "Beautifier.indentation" $ do
    context "When given negative indentation level" $ do
      it "should not error" $ do
        B.indentation (-1) <> "{" `shouldBe` "{"

    context "When given 0 indentation level" $ do
      it "should not indentation" $ do
        B.indentation 0 <> "{" `shouldBe` "{"

    context "When given 2 indentation level" $ do
      it "should indentation by 2 levels" $ do
        B.indentation 2 <> "{" `shouldBe` "    {"

  describe "Beautifier.splitAtHead" $ do
    context "when text first character is escaped" $ do
      it "should split escaped character as the head" $ do
        B.splitAtHead "\\\"hithere" `shouldBe` ("\\\"", "hithere")

    context "when the first character is not escaped" $ do
      it "should split correctly" $ do
        B.splitAtHead "testtest" `shouldBe` ("t", "esttest")

    context "when given empty text" $ do
      it "should split into empty strings" $ do
        B.splitAtHead "" `shouldBe` ("", "")
