module Data.TokenUsageLikelihood.Types
    ( RemovalLikelihood(..)
    , TokenRemoval(..)
    ) where

import qualified Data.Text as T
import Data.TokenUsageLikelihood.TokenOccurrences

data RemovalLikelihood
    = HighRemovalLikelihood T.Text
    | MediumRemovalLikelihood T.Text
    | LowRemovalLikelihood T.Text
    | AutomaticLowRemovalLikelihood T.Text
    | UnknownRemovalLikelihood
    deriving (Show, Eq)

data TokenRemoval = TokenRemoval
    { occurrences :: Occurrences
    , removalLikelihood :: RemovalLikelihood
    }
