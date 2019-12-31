module Data.TokenUsageLikelihood.Types
    ( RemovalLikelihood(..)
    ) where

import qualified Data.Text as T

data RemovalLikelihood
    = HighRemovalLikelihood T.Text
    | MediumRemovalLikelihood T.Text
    | LowRemovalLikelihood T.Text
    | AutomaticLowRemovalLikelihood T.Text
    | UnknownRemovalLikelihood
    deriving (Show, Eq)
