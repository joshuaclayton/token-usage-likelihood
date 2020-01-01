module Data.TokenUsageLikelihood.ProjectConfiguration
    ( ProjectConfiguration(..)
    , AutomaticLowLikelihood(..)
    , Matcher(..)
    , FileType(..)
    , decorateFilePath
    ) where

import Control.Applicative ((<|>))
import qualified Data.Maybe as M
import qualified Data.Text as T
import Data.TokenUsageLikelihood.TextPredicateMatch

data ProjectConfiguration = ProjectConfiguration
    { configurationName :: T.Text
    , applicationCodeDirectories :: [T.Text]
    , testCodeDirectories :: [T.Text]
    , configCodeDirectories :: [T.Text]
    , automaticLowLikelihood :: [AutomaticLowLikelihood]
    }

data Matcher
    = PathMatch PredicateOperation
    | TokenMatch PredicateOperation
    | FileTypeOccurrenceMatch FileType
                              Int

data AutomaticLowLikelihood = AutomaticLowLikelihood
    { automaticLowLikelihoodName :: T.Text
    , automaticLowLikelihoodMatchers :: [Matcher]
    }

data FileType
    = ApplicationFile
    | TestFile
    | ConfigFile
    | Unknown
    deriving (Eq)

decorateFilePath :: ProjectConfiguration -> FilePath -> FileType
decorateFilePath ProjectConfiguration { applicationCodeDirectories = app
                                      , testCodeDirectories = test
                                      , configCodeDirectories = config
                                      } path = M.fromMaybe Unknown go
  where
    go =
        calculateFileType path app ApplicationFile <|>
        calculateFileType path test TestFile <|>
        calculateFileType path config ConfigFile

calculateFileType :: FilePath -> [T.Text] -> FileType -> Maybe FileType
calculateFileType path paths defaultType =
    if any (check (T.pack path) . StartsWith) paths
        then Just defaultType
        else Nothing
