module Types
    ( Article(..)
    , Date(..)
    , Channel(..)
    )
where

import Data.Text (Text)

data Date = Date
    { dateYear :: Int
    , dateMonth :: Int
    , dateDay :: Int
    , dateHour :: Int
    , dateMinute :: Int
    , dateSecond :: Int
    } deriving (Show)

data Article = Article
    { articleTitle :: Text
    , articleLink :: Text
    , articleDescription :: Text
    , articleDate :: Maybe Date
    } deriving (Show)

data Channel = Channel
    { channelTitle :: Text
    , channelLink :: Text
    , channelDescription :: Text
    , channelArticles :: [Article]
    } deriving (Show)

