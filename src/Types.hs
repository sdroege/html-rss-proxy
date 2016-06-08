{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Types
    ( Article(..)
    , Date(..)
    , Channel(..)
    )
where

import Data.Text (Text)

import GHC.Generics (Generic)
import Data.Serialize
import Data.Serialize.Text

data Date = Date
    { dateYear :: Int
    , dateMonth :: Int
    , dateDay :: Int
    , dateHour :: Int
    , dateMinute :: Int
    , dateSecond :: Int
    } deriving (Show, Eq, Generic, Serialize)

data Article = Article
    { articleTitle :: Text
    , articleLink :: Text
    , articleDescription :: Text
    , articleDate :: Maybe Date
    } deriving (Show, Eq, Generic, Serialize)

data Channel = Channel
    { channelTitle :: Text
    , channelLink :: Text
    , channelDescription :: Text
    , channelArticles :: [Article]
    } deriving (Show, Eq, Generic, Serialize)


