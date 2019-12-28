module Data.TokenOccurrences
    ( TokenAndOccurrences(..)
    , ProjectConfiguration(..)
    , Output
    , Input(..)
    , TokenOccurrences(..)
    , processInput
    , totalOccurrences
    , unknownOccurrences
    , configDirectoryOccurrences
    , testDirectoryOccurrences
    , applicationDirectoryOccurrences
    ) where

import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.TokenOccurrences.Internal
import Data.TokenOccurrences.ProjectConfiguration
import Data.TokenOccurrences.Types
import qualified GHC.Natural as Nat

processInput :: ProjectConfiguration -> Input a FilePath -> Output a FilePath
processInput config input' =
    Output
        input'
        (go ApplicationFile)
        (go TestFile)
        (go ConfigFile)
        (go Unknown)
  where
    withDecoratedFilePaths = mapFilePaths (decorateFilePath config) input'
    go ft =
        handle calculateOccurrences $
        filterInput (\md -> fileType md == ft) withDecoratedFilePaths
    handle f (Token occ) = f occ
    handle f (TokenAndAlias occ ali) = f occ <> f ali

totalOccurrences :: Output a b -> TokenOccurrences
totalOccurrences o =
    foldl1
        mappend
        [ applicationDirectoryOccurrences o
        , testDirectoryOccurrences o
        , configDirectoryOccurrences o
        , unknownOccurrences o
        ]

calculateOccurrences :: TokenAndOccurrences a b -> TokenOccurrences
calculateOccurrences (TokenAndOccurrences _ map') =
    TokenOccurrences
        (Nat.intToNatural $ length $ HashMap.keys map')
        (sum $ HashMap.elems map')

filterInput :: (b -> Bool) -> Input a b -> Input a b
filterInput f (Token a) = Token (filterTokenAndOccurrences f a)
filterInput f (TokenAndAlias a b) =
    TokenAndAlias
        (filterTokenAndOccurrences f a)
        (filterTokenAndOccurrences f b)

filterTokenAndOccurrences ::
       (b -> Bool) -> TokenAndOccurrences a b -> TokenAndOccurrences a b
filterTokenAndOccurrences f (TokenAndOccurrences a b) =
    TokenAndOccurrences a (HashMap.filterWithKey (\k _ -> f k) b)

mapFilePaths ::
       (Hashable b, Hashable c, Eq b, Eq c)
    => (b -> c)
    -> Input a b
    -> Input a c
mapFilePaths f (Token (TokenAndOccurrences a' b')) =
    Token $ TokenAndOccurrences a' (mapKeys f b')
mapFilePaths f (TokenAndAlias (TokenAndOccurrences a' b') (TokenAndOccurrences a'' b'')) =
    TokenAndAlias
        (TokenAndOccurrences a' (mapKeys f b'))
        (TokenAndOccurrences a'' (mapKeys f b''))
