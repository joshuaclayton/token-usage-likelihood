module Data.TokenOccurrencesSpec where

import qualified Data.HashMap.Strict as HashMap
import Data.TokenOccurrences
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Data.TokenOccurrences" $
    it "calculates occurrences correctly" $ do
        let outcome = processInput railsConfiguration results
        fileCount (totalOccurrences outcome) `shouldBe` 4
        occurrenceCount (totalOccurrences outcome) `shouldBe` 13
        fileCount (applicationDirectoryOccurrences outcome) `shouldBe` 1
        occurrenceCount (applicationDirectoryOccurrences outcome) `shouldBe` 1
        fileCount (testDirectoryOccurrences outcome) `shouldBe` 2
        occurrenceCount (testDirectoryOccurrences outcome) `shouldBe` 11
        fileCount (configDirectoryOccurrences outcome) `shouldBe` 0
        occurrenceCount (configDirectoryOccurrences outcome) `shouldBe` 0
        fileCount (unknownOccurrences outcome) `shouldBe` 1
        occurrenceCount (unknownOccurrences outcome) `shouldBe` 1

results :: Input String
results =
    Token
        (TokenAndOccurrences
             "Person"
             (HashMap.fromList
                  [ ("app/models/person.rb", 1)
                  , ("spec/models/person_spec.rb", 10)
                  , ("spec/factories.rb", 1)
                  , ("data/seed.csv", 1)
                  ]))

railsConfiguration :: ProjectConfiguration
railsConfiguration =
    ProjectConfiguration
        { configurationName = "Rails"
        , applicationCodeDirectories = ["app/", "lib/"]
        , testCodeDirectories = ["spec/", "test/", "features/"]
        , configCodeDirectories = ["config/", "db/"]
        , automaticLowLikelihood = []
        }
