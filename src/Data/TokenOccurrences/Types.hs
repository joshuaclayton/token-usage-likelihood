module Data.TokenOccurrences.Types
    ( TokenAndOccurrences(..)
    , Output(..)
    , Input(..)
    , TokenOccurrences(..)
    ) where

import qualified Data.HashMap.Strict as HashMap
import Numeric.Natural (Natural)

data Input a b
    = Token (TokenAndOccurrences a b)
    | TokenAndAlias (TokenAndOccurrences a b)
                    (TokenAndOccurrences a b)

data Output a b = Output
    { input :: Input a b
    , applicationDirectoryOccurrences :: TokenOccurrences
    , testDirectoryOccurrences :: TokenOccurrences
    , configDirectoryOccurrences :: TokenOccurrences
    , unknownOccurrences :: TokenOccurrences
    }

data TokenAndOccurrences a b =
    TokenAndOccurrences a
                        (HashMap.HashMap b Natural)

instance Semigroup TokenOccurrences where
    (TokenOccurrences fc1 oc1) <> (TokenOccurrences fc2 oc2) =
        TokenOccurrences (fc1 + fc2) (oc1 + oc2)

instance Monoid TokenOccurrences where
    mempty = TokenOccurrences 0 0

data TokenOccurrences = TokenOccurrences
    { fileCount :: Natural
    , occurrenceCount :: Natural
    } deriving (Show, Eq)
