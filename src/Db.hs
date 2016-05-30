{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, TypeFamilies, TemplateHaskell #-}

module Db
    ( Channels(..)
    , GetChannel(..)
    , UpdateChannel(..)
    )
where

import Types
import Utils
import Config

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import GHC.Generics (Generic)
import Data.Data (Data)
import Control.DeepSeq
import Data.Typeable
import Data.SafeCopy
import Data.Acid

newtype Channels = Channels { getChannels :: Map Text Channel }
    deriving (Show, Typeable, Data, Generic, NFData)

$(deriveSafeCopy 0 'base ''Channels)

getChannel :: Text -> Query Channels (Maybe Channel)
getChannel name = do
    channels <- getChannels <$> ask
    return (M.lookup name channels)

updateChannel :: Text -> Date -> Channel -> Update Channels ()
updateChannel name date newChannel = do
    channels <- getChannels <$> get
    let newChannels = M.alter mergeChannel name channels
    newChannels `deepseq` put (Channels newChannels)
  where
    mergeChannel Nothing = Just (setChannelArticleDate date newChannel)
    mergeChannel (Just oldChannel) = Just mergedChannel
      where
        mergedChannel = setChannelArticleDate date . pruneOldChannelArticles maxChannelSize . mergeChannelArticles oldChannel $ newChannel

$(makeAcidic ''Channels
  [ 'getChannel
  , 'updateChannel
  ])

