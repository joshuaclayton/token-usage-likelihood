module Data.TokenUsageLikelihoodSpec where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.TextPredicateMatch
import Data.TokenOccurrences
import Data.TokenOccurrences.ProjectConfiguration
import Data.TokenUsageLikelihood as L
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Data.TokenUsageLikelihood" $ do
        it
            "calculates high removal likelihood when only the definition and tests exist" $ do
            let outcome =
                    calculateTokenUsage railsConfiguration definitionAndTest
            outcome `shouldBe`
                HighRemovalLikelihood
                    "only the definition and corresponding tests exist"
        it "calculates high removal likelihood when only the definition exists" $ do
            let outcome = calculateTokenUsage railsConfiguration onlyDefinition
            outcome `shouldBe`
                HighRemovalLikelihood "only the definition exists"
        it "calculates high removal likelihood when only a test helper exists" $ do
            let outcome =
                    calculateTokenUsage railsConfiguration onlyTestDefinition
            outcome `shouldBe`
                HighRemovalLikelihood "only the test definition exists"
        it
            "calculates moderate removal likelihood when a token exists multiple times but only in one file" $ do
            let outcome =
                    calculateTokenUsage
                        phoenixConfiguration
                        patternMatchedDefinition
            outcome `shouldBe`
                MediumRemovalLikelihood
                    "used multiple times but only in one file"
        it
            "calculates moderate removal likelihood when a token occurs a few times across only one app file and multiple other files" $ do
            let outcome =
                    calculateTokenUsage railsConfiguration moderateUseDefinition
            outcome `shouldBe`
                MediumRemovalLikelihood
                    "used multiple times but only in one application file"
        it
            "calculates low removal likelihood when a token occurs across the codebase" $ do
            let outcome =
                    calculateTokenUsage railsConfiguration widelyUsedDefinition
            outcome `shouldBe`
                LowRemovalLikelihood "widely used across the codebase"
        it
            "calculates low removal likelihood when multiple criteria are met for automatic low likelihood" $ do
            let outcome = calculateTokenUsage railsConfiguration railsMigration
            outcome `shouldBe`
                AutomaticLowRemovalLikelihood
                    "Automatic low likelihood: detected as Rails Migration"

definitionAndTest :: Input T.Text
definitionAndTest =
    TokenAndAlias
        (TokenAndOccurrences
             "admin?"
             (HashMap.fromList [("app/models/person.rb", 1)]))
        (TokenAndOccurrences
             "is_admin"
             (HashMap.fromList [("spec/models/person_spec.rb", 3)]))

onlyDefinition :: Input T.Text
onlyDefinition =
    Token
        (TokenAndOccurrences
             "with_comments"
             (HashMap.fromList [("app/models/post.rb", 1)]))

onlyTestDefinition :: Input T.Text
onlyTestDefinition =
    Token
        (TokenAndOccurrences
             "generate_test_data"
             (HashMap.fromList [("spec/support/fixture_helpers.rb", 1)]))

moderateUseDefinition :: Input T.Text
moderateUseDefinition =
    Token
        (TokenAndOccurrences
             "non_admins"
             (HashMap.fromList
                  [ ("app/models/user.rb", 1)
                  , ("db/seeds/dev.rb", 2)
                  , ("spec/models/user_spec.rb", 3)
                  ]))

widelyUsedDefinition :: Input T.Text
widelyUsedDefinition =
    Token
        (TokenAndOccurrences
             "full_name"
             (HashMap.fromList
                  [ ("app/models/user.rb", 3)
                  , ("app/views/application/_auth_header.rb", 1)
                  , ("app/mailers/user_mailer.rb", 1)
                  , ("spec/models/user_spec.rb", 5)
                  ]))

patternMatchedDefinition :: Input T.Text
patternMatchedDefinition =
    Token
        (TokenAndOccurrences
             "by_user"
             (HashMap.fromList [("web/models/post.ex", 2)]))

railsMigration :: Input T.Text
railsMigration =
    Token
        (TokenAndOccurrences
             "CreatePeople"
             (HashMap.fromList
                  [("db/migrate/20200101000000_create_people.rb", 1)]))

railsConfiguration :: ProjectConfiguration
railsConfiguration =
    ProjectConfiguration
        { configurationName = "Rails"
        , applicationCodeDirectories = ["app/", "lib/"]
        , testCodeDirectories = ["spec/", "test/", "features/"]
        , configCodeDirectories = ["config/", "db/"]
        , automaticLowLikelihood =
              [AutomaticLowLikelihood "Migration" railsMigrationChecks]
        }

phoenixConfiguration :: ProjectConfiguration
phoenixConfiguration =
    ProjectConfiguration
        { configurationName = "Phoenix"
        , applicationCodeDirectories = ["web/", "lib/"]
        , testCodeDirectories = ["test/"]
        , configCodeDirectories = ["config/"]
        , automaticLowLikelihood = []
        }

railsMigrationChecks :: [Matcher]
railsMigrationChecks =
    [ TokenMatch StartsWithCapital
    , PathMatch $ StartsWith "db/migrate/"
    , PathMatch $ EndsWith ".rb"
    , FileTypeOccurrenceMatch ConfigFile 1
    ]
