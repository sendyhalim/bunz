{-# LANGUAGE OverloadedStrings #-}

module BeautifierSpec (main, spec) where

import qualified Beautifier as B
import           Test.Hspec

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "Beautifier.indent" $ do
    context "When given negative indent level" $ do
      it "should not indent" $ do
        B.indent (-1) "{" `shouldBe` "{"

    context "When given 0 indent level" $ do
      it "should not indent" $ do
        B.indent 0 "{" `shouldBe` "{"

    context "When given 2 indent level" $ do
      it "should indent by 2 levels" $ do
        B.indent 2 "{" `shouldBe` "    {"

