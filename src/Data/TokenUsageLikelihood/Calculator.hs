module Data.TokenUsageLikelihood.Calculator
    ( analyze
    ) where

import Data.TokenUsageLikelihood.Internal
import Data.TokenUsageLikelihood.ProjectConfiguration
import Data.TokenUsageLikelihood.TokenOccurrences
import Data.TokenUsageLikelihood.Types

analyze :: ProjectConfiguration -> Input -> TokenRemoval
analyze config input' =
    TokenRemoval
        (processInput config input')
        (calculateTokenUsage config input')

calculateTokenUsage :: ProjectConfiguration -> Input -> RemovalLikelihood
calculateTokenUsage config input' =
    case autoLowLikelihood of
        Nothing
            | oneApplicationOccurrence occurrences' &&
                  noTestOccurrences occurrences' ->
                HighRemovalLikelihood "only the definition exists"
            | oneApplicationOccurrence occurrences' &&
                  occurrenceCountByFileType ConfigFile occurrences' == 0 &&
                  occurrenceCountByFileType TestFile occurrences' > 0 ->
                HighRemovalLikelihood
                    "only the definition and corresponding tests exist"
            | occursInOneFile occurrences' && oneTestOccurrence occurrences' ->
                HighRemovalLikelihood "only the test definition exists"
            | occursInOneFile occurrences' &&
                  noTestOccurrences occurrences' &&
                  occursMoreThanOnce occurrences' ->
                MediumRemovalLikelihood
                    "used multiple times but only in one file"
            | oneApplicationOccurrence occurrences' &&
                  occursMoreThanOnce occurrences' ->
                MediumRemovalLikelihood
                    "used multiple times but only in one application file"
            | totalFileCount occurrences' > 2 ->
                LowRemovalLikelihood "widely used across the codebase"
            | otherwise -> UnknownRemovalLikelihood
        Just autoLow ->
            AutomaticLowRemovalLikelihood $
            "Automatic low likelihood: detected as " <> configurationName config <>
            " " <>
            automaticLowLikelihoodName autoLow
  where
    autoLowLikelihood =
        matchesAutomaticLowLikelihood
            (automaticLowLikelihood config)
            input'
            occurrences'
    occurrences' = processInput config input'
