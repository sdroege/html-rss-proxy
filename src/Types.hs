module Types
    ( Article(..)
    , Channel(..)
    )
where

import Data.Text (Text)

data Article = Article
    { articleTitle :: Text
    , articleLink :: Text
    , articleDescription :: Text
    } deriving (Show)

data Channel = Channel
    { channelTitle :: Text
    , channelLink :: Text
    , channelDescription :: Text
    , channelArticles :: [Article]
    } deriving (Show)

