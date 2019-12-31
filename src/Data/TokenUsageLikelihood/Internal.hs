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

noTestOccurrences :: Output a b -> Bool
noTestOccurrences output = occurrenceCountByFileType TestFile output == 0

oneTestOccurrence :: Output a b -> Bool
oneTestOccurrence output = occurrenceCountByFileType TestFile output == 1

oneApplicationOccurrence :: Output a b -> Bool
oneApplicationOccurrence output =
    occurrenceCountByFileType ApplicationFile output == 1

occursInOneFile :: Output a b -> Bool
occursInOneFile output = totalFileCount output == 1

occursMoreThanOnce :: Output a b -> Bool
occursMoreThanOnce output = totalOccurrenceCount output > 1

matchesAutomaticLowLikelihood ::
       [AutomaticLowLikelihood]
    -> Output T.Text FilePath
    -> Maybe AutomaticLowLikelihood
matchesAutomaticLowLikelihood lowLikelihoods output =
    safeHead $ filter (matchesAll output) lowLikelihoods
  where
    safeHead [] = Nothing
    safeHead (x:_) = Just x
    matchesAll o ll = all (matchesOutput o) (automaticLowLikelihoodMatchers ll)

matchesOutput :: Output T.Text FilePath -> Matcher -> Bool
matchesOutput output matcher =
    case matcher of
        TokenMatch pre -> any (`check` pre) $ inputTokenNames $ input output
        PathMatch pre ->
            any (\n -> check (T.pack n) pre) $ inputFilePaths $ input output
        FileTypeOccurrenceMatch ft count ->
            fromIntegral (fileCountByFileType ft output) == count

fileCountByFileType :: FileType -> Output a b -> Natural
fileCountByFileType ft = fileCount . tokenOccurrencesByFileType ft

occurrenceCountByFileType :: FileType -> Output a b -> Natural
occurrenceCountByFileType ft = occurrenceCount . tokenOccurrencesByFileType ft

totalFileCount :: Output a b -> Natural
totalFileCount = fileCount . totalOccurrences

totalOccurrenceCount :: Output a b -> Natural
totalOccurrenceCount = occurrenceCount . totalOccurrences

tokenOccurrencesByFileType :: FileType -> Output a b -> TokenOccurrences
tokenOccurrencesByFileType ApplicationFile = applicationDirectoryOccurrences
tokenOccurrencesByFileType TestFile = testDirectoryOccurrences
tokenOccurrencesByFileType ConfigFile = configDirectoryOccurrences
tokenOccurrencesByFileType Unknown = unknownOccurrences
