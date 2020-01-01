module Data.TokenUsageLikelihood.TextPredicateMatch
    ( PredicateOperation(..)
    , check
    ) where

import qualified Data.Char as C
import qualified Data.Text as T

data PredicateOperation
    = StartsWith T.Text
    | EndsWith T.Text
    | Equals T.Text
    | StartsWithCapital

check :: T.Text -> PredicateOperation -> Bool
check haystack (StartsWith needle) = needle `T.isPrefixOf` haystack
check haystack (EndsWith needle) = needle `T.isSuffixOf` haystack
check haystack (Equals needle) = needle == haystack
check haystack StartsWithCapital =
    case T.uncons haystack of
        Nothing -> False
        Just (c, _) -> C.isUpper c
