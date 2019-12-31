module Data.TokenUsageLikelihood.Internal
    ( noTestOccurrences
    , totalFileCount
    , occurrenceCountByFileType
    , occursMoreThanOnce
    , oneTestOccurrence
    , occursInOneFile
    , matchesAutomaticLowLikelihood
    , oneApplicationOccurrence
    ) where

import qualified Data.Text as T
import Data.TextPredicateMatch
import Data.TokenOccurrences
import Data.TokenOccurrences.ProjectConfiguration
import Data.TokenOccurrences.Types
import Numeric.Natural (Natural)

noTestOccurrences :: Occurrences -> Bool
noTestOccurrences output = occurrenceCountByFileType TestFile output == 0

oneTestOccurrence :: Occurrences -> Bool
oneTestOccurrence output = occurrenceCountByFileType TestFile output == 1

oneApplicationOccurrence :: Occurrences -> Bool
oneApplicationOccurrence output =
    occurrenceCountByFileType ApplicationFile output == 1

occursInOneFile :: Occurrences -> Bool
occursInOneFile output = totalFileCount output == 1

occursMoreThanOnce :: Occurrences -> Bool
occursMoreThanOnce output = totalOccurrenceCount output > 1

matchesAutomaticLowLikelihood ::
       [AutomaticLowLikelihood]
    -> Input T.Text
    -> Occurrences
    -> Maybe AutomaticLowLikelihood
matchesAutomaticLowLikelihood lowLikelihoods input' occurrences' =
    safeHead $ filter (matchesAll input' occurrences') lowLikelihoods
  where
    safeHead [] = Nothing
    safeHead (x:_) = Just x
    matchesAll i o ll =
        all (matchesOutput i o) (automaticLowLikelihoodMatchers ll)

matchesOutput :: Input T.Text -> Occurrences -> Matcher -> Bool
matchesOutput input' occurrences' matcher =
    case matcher of
        TokenMatch pre -> any (`check` pre) $ inputTokenNames input'
        PathMatch pre ->
            any (\n -> check (T.pack n) pre) $ inputFilePaths input'
        FileTypeOccurrenceMatch ft count ->
            fromIntegral (fileCountByFileType ft occurrences') == count

fileCountByFileType :: FileType -> Occurrences -> Natural
fileCountByFileType ft = fileCount . tokenOccurrencesByFileType ft

occurrenceCountByFileType :: FileType -> Occurrences -> Natural
occurrenceCountByFileType ft = occurrenceCount . tokenOccurrencesByFileType ft

totalFileCount :: Occurrences -> Natural
totalFileCount = fileCount . totalOccurrences

totalOccurrenceCount :: Occurrences -> Natural
totalOccurrenceCount = occurrenceCount . totalOccurrences

tokenOccurrencesByFileType :: FileType -> Occurrences -> TokenOccurrences
tokenOccurrencesByFileType ApplicationFile = applicationDirectoryOccurrences
tokenOccurrencesByFileType TestFile = testDirectoryOccurrences
tokenOccurrencesByFileType ConfigFile = configDirectoryOccurrences
tokenOccurrencesByFileType Unknown = unknownOccurrences
