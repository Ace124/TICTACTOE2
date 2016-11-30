import Test.Hspec
import Data.Maybe
import Validate
import Defense
import Parser

main :: IO ()
main = hspec $ do
  describe "Validation of game that is already won" $ do
    it "Should return True because o won" $ do
      validate "[[\"x\",   0,   \"y\",  2,   \"v\",  \"x\"],   [\"x\",   0,   \"y\",   0,   \"v\",  \"o\"], [\"x\",   2,  \"y\", 0, \"v\", \"x\"],  [\"x\", 1, \"y\", 1, \"v\",   \"o\"],   [\"x\",  0, \"y\",  1,  \"v\",   \"x\"], [\"x\",  2, \"y\",   2,   \"v\", \"o\"]]" 
      `shouldBe` True 
  describe "Validation of game that is not ower(no winners)" $ do
    it "Should return False because nobody won" $ do
      validate "[[\"x\", 2,  \"y\",  2,   \"v\",  \"x\"],  [\"x\", 1,  \"y\",   2,   \"v\",  \"o\"], [\"x\", 0,   \"y\",   1,  \"v\",  \"x\"], [\"x\", 1,   \"y\",   0, \"v\", \"o\"],   [\"x\",  0,   \"y\", 0, \"v\",   \"x\"], [\"x\",  2,   \"y\",  1,   \"v\",   \"o\"],  [\"x\", 2, \"y\",   0,  \"v\",  \"x\"],   [\"x\",  0,   \"y\",   2, \"v\", \"o\"]]" 
      `shouldBe` False
  describe "Check to see if parsing correctly" $ do
    it "Should return [('0','1','x'),('0','2','o'),('1','2','x'),('1','0','o'),('2','2','x'),('0','0','o'),('2','1','x')]" $ do
      parseJson "[[\"x\",  0,  \"y\", 1, \"v\",   \"x\"],  [\"x\",   0, \"y\", 2,  \"v\", \"o\"],   [\"x\", 1,  \"y\",  2,   \"v\",   \"x\"],  [\"x\", 1, \"y\",  0,  \"v\",  \"o\"],   [\"x\",   2,  \"y\", 2, \"v\",  \"x\"],  [\"x\",   0,  \"y\",  0,   \"v\",  \"o\"],   [\"x\", 2,   \"y\",  1,   \"v\",   \"x\"]]"
      `shouldBe` [('0','1','x'),('0','2','o'),('1','2','x'),('1','0','o'),('2','2','x'),('0','0','o'),('2','1','x')]
  describe "Check if encoding move to Json correctly" $ do
    it "Should return Just [\"x\", 2, \"y\", 0, \"v\", \"o\"]]" $ do
      encodeMove $ move "[[\"x\",  0,  \"y\", 1, \"v\",   \"x\"],  [\"x\",   0, \"y\", 2,  \"v\", \"o\"],   [\"x\", 1,  \"y\",  2,   \"v\",   \"x\"],  [\"x\", 1, \"y\",  0,  \"v\",  \"o\"],   [\"x\",   2,  \"y\", 2, \"v\",  \"x\"],  [\"x\",   0,  \"y\",  0,   \"v\",  \"o\"],   [\"x\", 2,   \"y\",  1,   \"v\",   \"x\"]]"
      `shouldBe` Just "[\"x\", 2, \"y\", 0, \"v\", \"o\"]]"
  describe "Wins the game if it is possible" $ do
    it "Should return Just(2,0,'o')" $ do
      tryToWin $ parseJson "[[\"x\",  0,  \"y\", 1, \"v\",   \"x\"],  [\"x\",   0, \"y\", 2,  \"v\", \"o\"],   [\"x\", 1,  \"y\",  2,   \"v\",   \"x\"],  [\"x\", 1, \"y\",  0,  \"v\",  \"o\"],   [\"x\",   2,  \"y\", 2, \"v\",  \"x\"],  [\"x\",   0,  \"y\",  0,   \"v\",  \"o\"],   [\"x\", 2,   \"y\",  1,   \"v\",   \"x\"]]" 
      `shouldBe` Just(2,0,'o')
   