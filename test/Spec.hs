import Test.Hspec
import Data.Maybe
import Validate

main :: IO ()
main = hspec $ do
  describe "Validates" $ do
    it "Should return True because o won" $ do
      validate "[[\"x\",   0,   \"y\",  2,   \"v\",  \"x\"],   [\"x\",   0,   \"y\",   0,   \"v\",  \"o\"], [\"x\",   2,  \"y\", 0, \"v\", \"x\"],  [\"x\", 1, \"y\", 1, \"v\",   \"o\"],   [\"x\",  0, \"y\",  1,  \"v\",   \"x\"], [\"x\",  2, \"y\",   2,   \"v\", \"o\"]]" `shouldBe` True 