module Data.TokenUsageLikelihood
    ( RemovalLikelihood(..)
    , TokenRemoval(..)
    , analyze
    ) where

import Data.TokenOccurrences
import Data.TokenUsageLikelihood.Calculator
import Data.TokenUsageLikelihood.Types

analyze :: ProjectConfiguration -> Input -> TokenRemoval
analyze config input' =
    TokenRemoval
        input'
        (processInput config input')
        (calculateTokenUsage config input')
