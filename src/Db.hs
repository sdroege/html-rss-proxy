{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, TypeFamilies, TemplateHaskell #-}

module Db
    ( Channels(..)
    , GetChannel(..)
    , UpdateChannel(..)
    , getChannel'
    , updateChannel'
    )
where

import Types
import Utils
import Config

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import GHC.Generics (Generic)
import Data.Data (Data)
import Control.DeepSeq
import Data.Typeable
import Data.SafeCopy
import Data.Acid

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import System.FilePath ((</>))

import Data.Serialize hiding (get, put)

import Control.Exception
import System.IO.Error

import System.FileLock
import System.AtomicWrite.Writer.LazyByteString

newtype Channels = Channels { getChannels :: Map Text Channel }
    deriving (Show, Eq, Typeable, Data, Generic, NFData)

$(deriveSafeCopy 0 'base ''Channels)

getChannel :: Text -> Query Channels (Maybe Channel)
getChannel name = do
    channels <- getChannels <$> ask
    return (M.lookup name channels)

updateChannel :: Text -> Date -> Channel -> Update Channels ()
updateChannel name date newChannel = do
    channels <- getChannels <$> get
    let oldChannel = M.lookup name channels
        mergedChannel = mergeChannel oldChannel newChannel

    when (Just mergedChannel /= oldChannel) $ do
        let newChannels = M.insert name mergedChannel channels
        newChannels `deepseq` put (Channels newChannels)

  where
    mergeChannel Nothing = setChannelArticleDate date
    mergeChannel (Just oldChannel) = setChannelArticleDate date . pruneOldChannelArticles maxChannelSize . mergeChannelArticles oldChannel

$(makeAcidic ''Channels
  [ 'getChannel
  , 'updateChannel
  ])

eitherToMaybe :: Either l r -> Maybe r
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right r) = Just r

buildChannelFilename :: FilePath -> Text -> FilePath
buildChannelFilename path name = path </> (T.unpack name ++ ".dat")

getChannel' :: FilePath -> Text -> IO (Maybe Channel)
getChannel' path name = do
    let filename = buildChannelFilename path name

    withFileLock filename Shared $ \_ -> do
        content <- handleJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                        (\_ -> return Nothing)
                        (Just <$> B.readFile filename)

        return (join (eitherToMaybe . decodeLazy <$> content))

updateChannel' :: FilePath -> Text -> Maybe Date -> Channel -> IO ()
updateChannel' path name date newChannel = do
    let filename = buildChannelFilename path name

    withFileLock filename Exclusive $ \_ -> do
        content <- handleJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                        (\_ -> return Nothing)
                        (Just <$> B.readFile filename)

        let oldChannel = join (eitherToMaybe . decodeLazy <$> content)
            mergedChannel = mergeChannel oldChannel newChannel

        when (Just mergedChannel /= oldChannel) $ do
            let newContent = encodeLazy mergedChannel
            atomicWriteFile filename newContent

  where
    mergeChannel Nothing = maybeSetArticleDate date
    mergeChannel (Just oldChannel) = maybeSetArticleDate date . pruneOldChannelArticles maxChannelSize . mergeChannelArticles oldChannel
    maybeSetArticleDate Nothing = id
    maybeSetArticleDate (Just d)  = setChannelArticleDate d
