module Data.TokenUsageLikelihoodSpec where

import Data.TokenUsageLikelihood
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Data.TokenUsageLikelihood" $ do
        it
            "calculates high removal likelihood when only the definition and tests exist" $ do
            let outcome =
                    calculateLikelihood railsConfiguration definitionAndTest
            outcome `shouldBe`
                HighRemovalLikelihood
                    "only the definition and corresponding tests exist"
        it "calculates high removal likelihood when only the definition exists" $ do
            let outcome = calculateLikelihood railsConfiguration onlyDefinition
            outcome `shouldBe`
                HighRemovalLikelihood "only the definition exists"
        it "calculates high removal likelihood when only a test helper exists" $ do
            let outcome =
                    calculateLikelihood railsConfiguration onlyTestDefinition
            outcome `shouldBe`
                HighRemovalLikelihood "only the test definition exists"
        it
            "calculates moderate removal likelihood when a token exists multiple times but only in one file" $ do
            let outcome =
                    calculateLikelihood
                        phoenixConfiguration
                        patternMatchedDefinition
            outcome `shouldBe`
                MediumRemovalLikelihood
                    "used multiple times but only in one file"
        it
            "calculates moderate removal likelihood when a token occurs a few times across only one app file and multiple other files" $ do
            let outcome =
                    calculateLikelihood railsConfiguration moderateUseDefinition
            outcome `shouldBe`
                MediumRemovalLikelihood
                    "used multiple times but only in one application file"
        it
            "calculates low removal likelihood when a token occurs across the codebase" $ do
            let outcome =
                    calculateLikelihood railsConfiguration widelyUsedDefinition
            outcome `shouldBe`
                LowRemovalLikelihood "widely used across the codebase"
        it
            "calculates low removal likelihood when multiple criteria are met for automatic low likelihood" $ do
            let outcome = calculateLikelihood railsConfiguration railsMigration
            outcome `shouldBe`
                AutomaticLowRemovalLikelihood
                    "Automatic low likelihood: detected as Rails Migration"

calculateLikelihood :: ProjectConfiguration -> Input -> RemovalLikelihood
calculateLikelihood config = removalLikelihood . analyze config

definitionAndTest :: Input
definitionAndTest = isAdmin `isAnAliasOf` adminPredicate
  where
    adminPredicate = tokenWithOccurrences "admin?" [("app/models/person.rb", 1)]
    isAdmin =
        tokenWithOccurrences "is_admin" [("spec/models/person_spec.rb", 3)]

onlyDefinition :: Input
onlyDefinition =
    tokenWithOccurrences "with_comments" [("app/models/post.rb", 1)]

onlyTestDefinition :: Input
onlyTestDefinition =
    tokenWithOccurrences
        "generate_test_data"
        [("spec/support/fixture_helpers.rb", 1)]

moderateUseDefinition :: Input
moderateUseDefinition =
    tokenWithOccurrences
        "non_admins"
        [ ("app/models/user.rb", 1)
        , ("db/seeds/dev.rb", 2)
        , ("spec/models/user_spec.rb", 3)
        ]

widelyUsedDefinition :: Input
widelyUsedDefinition =
    tokenWithOccurrences
        "full_name"
        [ ("app/models/user.rb", 3)
        , ("app/views/application/_auth_header.rb", 1)
        , ("app/mailers/user_mailer.rb", 1)
        , ("spec/models/user_spec.rb", 5)
        ]

patternMatchedDefinition :: Input
patternMatchedDefinition =
    tokenWithOccurrences "by_user" [("web/models/post.ex", 2)]

railsMigration :: Input
railsMigration =
    tokenWithOccurrences
        "CreatePeople"
        [("db/migrate/20200101000000_create_people.rb", 1)]

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
