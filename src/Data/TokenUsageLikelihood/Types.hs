module Data.TokenUsageLikelihood.Types
    ( RemovalLikelihood(..)
    , TokenRemoval(..)
    ) where

import qualified Data.Text as T
import Data.TokenOccurrences

data RemovalLikelihood
    = HighRemovalLikelihood T.Text
    | MediumRemovalLikelihood T.Text
    | LowRemovalLikelihood T.Text
    | AutomaticLowRemovalLikelihood T.Text
    | UnknownRemovalLikelihood
    deriving (Show, Eq)

data TokenRemoval a = TokenRemoval
    { input :: Input a
    , occurrences :: Occurrences
    , removalLikelihood :: RemovalLikelihood
    }
