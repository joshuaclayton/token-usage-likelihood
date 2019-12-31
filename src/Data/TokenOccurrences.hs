module Data.TokenOccurrences
    ( TokenAndOccurrences(..)
    , ProjectConfiguration(..)
    , Input(..)
    , TokenOccurrences
    , Occurrences
    , fileCount
    , occurrenceCount
    , processInput
    , totalOccurrences
    , unknownOccurrences
    , configDirectoryOccurrences
    , testDirectoryOccurrences
    , applicationDirectoryOccurrences
    ) where

import qualified Data.HashMap.Strict as HashMap
import Data.TokenOccurrences.ProjectConfiguration
import Data.TokenOccurrences.Types

processInput :: ProjectConfiguration -> Input -> Occurrences
processInput config input' =
    Occurrences (go ApplicationFile) (go TestFile) (go ConfigFile) (go Unknown)
  where
    go ft =
        handle calculateOccurrences $
        filterInput (\md -> decorateFilePath config md == ft) input'
    handle f (Token occ) = f occ
    handle f (TokenAndAlias occ ali) = f occ <> f ali

totalOccurrences :: Occurrences -> TokenOccurrences
totalOccurrences o =
    foldl1
        mappend
        [ applicationDirectoryOccurrences o
        , testDirectoryOccurrences o
        , configDirectoryOccurrences o
        , unknownOccurrences o
        ]

calculateOccurrences :: TokenAndOccurrences -> TokenOccurrences
calculateOccurrences (TokenAndOccurrences _ map') = buildTokenOccurrences map'

filterInput :: (FilePath -> Bool) -> Input -> Input
filterInput f (Token a) = Token (filterTokenAndOccurrences f a)
filterInput f (TokenAndAlias a b) =
    TokenAndAlias
        (filterTokenAndOccurrences f a)
        (filterTokenAndOccurrences f b)

filterTokenAndOccurrences ::
       (FilePath -> Bool) -> TokenAndOccurrences -> TokenAndOccurrences
filterTokenAndOccurrences f (TokenAndOccurrences a b) =
    TokenAndOccurrences a (HashMap.filterWithKey (\k _ -> f k) b)
