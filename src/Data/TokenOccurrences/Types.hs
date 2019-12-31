module Data.TokenOccurrences.Types
    ( TokenAndOccurrences(..)
    , Input(..)
    , TokenOccurrences
    , Occurrences(..)
    , fileCount
    , buildTokenOccurrences
    , occurrenceCount
    , inputTokenNames
    , inputFilePaths
    ) where

import qualified Data.HashMap.Strict as HashMap
import qualified GHC.Natural as Nat
import Numeric.Natural (Natural)

data Input a
    = Token (TokenAndOccurrences a)
    | TokenAndAlias (TokenAndOccurrences a)
                    (TokenAndOccurrences a)

data Occurrences = Occurrences
    { applicationDirectoryOccurrences :: TokenOccurrences
    , testDirectoryOccurrences :: TokenOccurrences
    , configDirectoryOccurrences :: TokenOccurrences
    , unknownOccurrences :: TokenOccurrences
    }

data TokenAndOccurrences a =
    TokenAndOccurrences a
                        (HashMap.HashMap FilePath Natural)

instance Semigroup TokenOccurrences where
    NoOccurrences <> NoOccurrences = NoOccurrences
    NoOccurrences <> to = to
    to <> NoOccurrences = to
    (TokenOccurrences fc1 oc1) <> (TokenOccurrences fc2 oc2) =
        TokenOccurrences (fc1 + fc2 + 1) (oc1 + oc2)

instance Monoid TokenOccurrences where
    mempty = NoOccurrences

data TokenOccurrences
    = NoOccurrences
    | TokenOccurrences { fileCountDelta :: Natural
                       , occurrenceCountDelta :: Natural }
    deriving (Show, Eq)

occurrenceCount :: TokenOccurrences -> Natural
occurrenceCount NoOccurrences = 0
occurrenceCount to = fileCount to + occurrenceCountDelta to

fileCount :: TokenOccurrences -> Natural
fileCount NoOccurrences = 0
fileCount to = 1 + fileCountDelta to

buildTokenOccurrences :: HashMap.HashMap a Natural -> TokenOccurrences
buildTokenOccurrences map' =
    case ( Nat.intToNatural $ length $ HashMap.keys map'
         , sum $ HashMap.elems map') of
        (0, _) -> NoOccurrences
        (fc, oc) -> TokenOccurrences (fc - 1) (oc - fc)

inputTokenNames :: Input a -> [a]
inputTokenNames (Token (TokenAndOccurrences t _)) = [t]
inputTokenNames (TokenAndAlias (TokenAndOccurrences t _) (TokenAndOccurrences a _)) =
    [t, a]

inputFilePaths :: Input a -> [FilePath]
inputFilePaths (Token (TokenAndOccurrences _ map')) = HashMap.keys map'
inputFilePaths (TokenAndAlias (TokenAndOccurrences _ map') (TokenAndOccurrences _ map'')) =
    HashMap.keys map' ++ HashMap.keys map''
