module Data.TokenOccurrences.Internal
    ( mapKeys
    ) where

import qualified Data.Bifunctor as BF
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)

mapKeys ::
       (Hashable k1, Hashable k2, Eq k1, Eq k2)
    => (k1 -> k2)
    -> HashMap.HashMap k1 v
    -> HashMap.HashMap k2 v
mapKeys f = HashMap.fromList . map (BF.first f) . HashMap.toList
