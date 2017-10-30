import qualified Character             as C
import           Test.Hspec
import           Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Character.isEscaped" $ do
    context "Given character \\t" $ do
      it "should return True" $ do
        C.isEscaped "\\t" `shouldBe` True

    context "Given character \t" $ do
      it "should return False" $ do
        C.isEscaped "\t" `shouldBe` False
