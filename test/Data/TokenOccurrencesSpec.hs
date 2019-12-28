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
        totalOccurrences outcome `shouldBe` TokenOccurrences 4 13
        applicationDirectoryOccurrences outcome `shouldBe` TokenOccurrences 1 1
        testDirectoryOccurrences outcome `shouldBe` TokenOccurrences 2 11
        configDirectoryOccurrences outcome `shouldBe` TokenOccurrences 0 0
        unknownOccurrences outcome `shouldBe` TokenOccurrences 1 1

results :: Input String FilePath
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
        }
