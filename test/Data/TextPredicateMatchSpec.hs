module Data.TextPredicateMatchSpec where

import Data.TextPredicateMatch
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Data.TextPredicateMatch" $ do
        context "StartsWith" $
            it "calculates values correctly" $ do
                check "foobar" (StartsWith "foo") `shouldBe` True
                check "foobar" (StartsWith "bar") `shouldBe` False
                check "" (StartsWith "bar") `shouldBe` False
                check "foo" (StartsWith "foo") `shouldBe` True
        context "EndsWith" $
            it "calculates values correctly" $ do
                check "foobar" (EndsWith "foo") `shouldBe` False
                check "foobar" (EndsWith "bar") `shouldBe` True
                check "" (EndsWith "bar") `shouldBe` False
                check "foo" (EndsWith "foo") `shouldBe` True
        context "Equals" $
            it "calculates values correctly" $ do
                check "foobar" (Equals "foo") `shouldBe` False
                check "foobar" (Equals "bar") `shouldBe` False
                check "" (Equals "") `shouldBe` True
                check "foo" (Equals "foo") `shouldBe` True
        context "StartsWithCapital" $
            it "calculates values correctly" $ do
                check "foobar" StartsWithCapital `shouldBe` False
                check "Foobar" StartsWithCapital `shouldBe` True
                check "" StartsWithCapital `shouldBe` False
                check "!" StartsWithCapital `shouldBe` False
                check "_ABC" StartsWithCapital `shouldBe` False
