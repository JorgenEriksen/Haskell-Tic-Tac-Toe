import Lib
import Test.DocTest(doctest)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck(property)

spec_opponents :: Spec
spec_opponents = do
  describe "returns the mode from number argument" $ do
    it "works with '1'" $ do
      opponents 1 `shouldBe` ('p','p')

spec_aiTurn :: Spec
spec_aiTurn = do
  describe "returns a turn command for AI if randam number equals 0 or 1" $ do
    it "works with '1'" $ do
      aiTurn 1 `shouldBe` " right"

spec_availableSlots :: Spec
spec_availableSlots = do
  describe "returns the available indexes in board" $ do
    it "works with ['_', 'x', '_', '_', '_', '_', 'o', '_', '_']" $ do
      availableSlots ['_', 'x', '_', '_', '_', '_', 'o', '_', '_'] `shouldBe` [0,2,3,4,5,7,8]

spec_rotateLeft :: Spec
spec_rotateLeft = do
  describe "rotates the board to the left" $ do
    it "works with ['_', 'x', '_', 'o', 'x', '_', 'o', '_', '_']" $ do
      rotateLeft ['_', 'x', '_', 'o', 'x', '_', 'o', '_', '_'] `shouldBe` "___xx__oo"

spec_rotateRight :: Spec
spec_rotateRight = do
  describe "rotates the board to the right" $ do
    it "works with ['_', 'x', 'x', '_', 'o', '_', 'o', '_', '_']" $ do
      rotateRight ['_', 'x', 'x', '_', 'o', '_', 'o', '_', '_'] `shouldBe` "o___ox__x"

spec_swap1and3 :: Spec
spec_swap1and3 = do
  describe "swaps position 1 and 3 (index 0 and 2) in board" $ do
    it "works with ['x', '_', 'o', 'o', '_', 'x', 'o', 'x', '_']" $ do
      swap1and3 ['x', '_', 'o', 'o', '_', 'x', 'o', 'x', '_'] `shouldBe` "o_xo_xox_"

spec_changePlayer :: Spec
spec_changePlayer = do
  describe "change the current player between x and o" $ do
    it "works with 'x'" $ do
      changePlayer 'x' `shouldBe` 'o'

spec_newBoard :: Spec
spec_newBoard = do
  describe "creates the new board with the new mark" $ do
    it "works with ['_', 'x', '_', '_', 'x', '_', 'o', '_', '_'] 3 'o'" $ do
      newBoard ['_', 'x', '_', '_', 'x', '_', 'o', '_', '_'] 3 'o' `shouldBe` "_x_ox_o__"

spec_blankBoard :: Spec
spec_blankBoard = do
  describe "creates the new board with the new mark" $ do
    it "return blank board" $ do
      blankBoard `shouldBe` ['_', '_', '_', '_', '_', '_', '_', '_', '_']

main :: IO ()
main = do
  doctest ["-isrc", "app/Main.hs"]
  hspec $ do
    spec_opponents
    spec_aiTurn
    spec_availableSlots
    spec_rotateLeft
    spec_rotateRight
    spec_changePlayer
    spec_newBoard
    spec_blankBoard
