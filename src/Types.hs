{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module Types
    ( Article(..)
    , Date(..)
    , Channel(..)
    )
where

import Data.Text (Text)

import GHC.Generics (Generic)
import Data.Data (Data)
import Control.DeepSeq (NFData)
import Data.Typeable
import Data.SafeCopy
import Data.Serialize
import Data.Serialize.Text

data Date = Date
    { dateYear :: Int
    , dateMonth :: Int
    , dateDay :: Int
    , dateHour :: Int
    , dateMinute :: Int
    , dateSecond :: Int
    } deriving (Show, Eq, Typeable, Data, Generic, Serialize, NFData)

$(deriveSafeCopy 0 'base ''Date)

data Article = Article
    { articleTitle :: Text
    , articleLink :: Text
    , articleDescription :: Text
    , articleDate :: Maybe Date
    } deriving (Show, Eq, Typeable, Data, Generic, Serialize, NFData)

$(deriveSafeCopy 0 'base ''Article)

data Channel = Channel
    { channelTitle :: Text
    , channelLink :: Text
    , channelDescription :: Text
    , channelArticles :: [Article]
    } deriving (Show, Eq, Typeable, Data, Generic, Serialize, NFData)

$(deriveSafeCopy 0 'base ''Channel)


