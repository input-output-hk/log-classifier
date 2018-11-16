module Main where

import           Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- TODO: Write real test cases

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Prelude.read" $ do
    it "can parse integers" $
      read "10" `shouldBe` (10 :: Int)

    it "can parse floating-point numbers" $
      read "2.5" `shouldBe` (2.5 :: Float)
