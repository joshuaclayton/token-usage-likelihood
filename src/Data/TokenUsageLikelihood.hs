module Data.TokenUsageLikelihood
    ( RemovalLikelihood(..)
    , calculateTokenUsage
    ) where

import qualified Data.Text as T
import Data.TokenOccurrences
import Data.TokenOccurrences.ProjectConfiguration
import Data.TokenUsageLikelihood.Internal
import Data.TokenUsageLikelihood.Types

calculateTokenUsage ::
       ProjectConfiguration -> Input T.Text FilePath -> RemovalLikelihood
calculateTokenUsage config input' =
    case autoLowLikelihood of
        Nothing
            | oneApplicationOccurrence output && noTestOccurrences output ->
                HighRemovalLikelihood "only the definition exists"
            | oneApplicationOccurrence output &&
                  occurrenceCountByFileType ConfigFile output == 0 &&
                  occurrenceCountByFileType TestFile output > 0 ->
                HighRemovalLikelihood
                    "only the definition and corresponding tests exist"
            | occursInOneFile output && oneTestOccurrence output ->
                HighRemovalLikelihood "only the test definition exists"
            | occursInOneFile output &&
                  noTestOccurrences output && occursMoreThanOnce output ->
                MediumRemovalLikelihood
                    "used multiple times but only in one file"
            | oneApplicationOccurrence output && occursMoreThanOnce output ->
                MediumRemovalLikelihood
                    "used multiple times but only in one application file"
            | totalFileCount output > 2 ->
                LowRemovalLikelihood "widely used across the codebase"
            | otherwise -> UnknownRemovalLikelihood
        Just autoLow ->
            AutomaticLowRemovalLikelihood $
            "Automatic low likelihood: detected as " <> configurationName config <>
            " " <>
            automaticLowLikelihoodName autoLow
  where
    autoLowLikelihood =
        matchesAutomaticLowLikelihood (automaticLowLikelihood config) output
    output = processInput config input'
