{-# LANGUAGE DeriveGeneric #-}

module Data.TokenOccurrences.ProjectConfiguration
    ( ProjectConfiguration(..)
    , FilePathWithMetadata(..)
    , FileType(..)
    , decorateFilePath
    ) where

import Control.Applicative ((<|>))
import Data.Hashable (Hashable)
import qualified Data.List as L
import qualified Data.Maybe as M
import GHC.Generics (Generic)

data ProjectConfiguration = ProjectConfiguration
    { configurationName :: String
    , applicationCodeDirectories :: [String]
    , testCodeDirectories :: [String]
    , configCodeDirectories :: [String]
    }

data FileType
    = ApplicationFile
    | TestFile
    | ConfigFile
    | Unknown
    deriving (Eq, Generic)

instance Hashable FileType

data FilePathWithMetadata = FilePathWithMetadata
    { filePath :: FilePath
    , fileType :: FileType
    } deriving (Eq, Generic)

instance Hashable FilePathWithMetadata

decorateFilePath :: ProjectConfiguration -> FilePath -> FilePathWithMetadata
decorateFilePath ProjectConfiguration { applicationCodeDirectories = app
                                      , testCodeDirectories = test
                                      , configCodeDirectories = config
                                      } path =
    FilePathWithMetadata path (M.fromMaybe Unknown go)
  where
    go =
        calculateFileType path app ApplicationFile <|>
        calculateFileType path test TestFile <|>
        calculateFileType path config ConfigFile

calculateFileType :: FilePath -> [String] -> FileType -> Maybe FileType
calculateFileType path paths defaultType =
    if any (`L.isPrefixOf` path) paths
        then Just defaultType
        else Nothing
