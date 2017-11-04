module CharacterSpec (main, spec) where

import qualified Character  as C
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Character.isEscaped" $ do
    context "Given character \\t" $ do
      it "should return True" $ do
        C.isEscaped "\\t" `shouldBe` True

    context "Given character \t" $ do
      it "should return False" $ do
        C.isEscaped "\t" `shouldBe` False
