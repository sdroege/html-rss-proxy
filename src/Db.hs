module Db
    ( getChannelFromDb
    , updateChannelInDb
    )
where

import Types
import Utils
import Config

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.ByteString.Lazy as B

import System.FilePath ((</>))

import Control.Monad
import Data.Serialize

import Control.Exception
import System.IO.Error

import System.FileLock
import System.AtomicWrite.Writer.LazyByteString

eitherToMaybe :: Either l r -> Maybe r
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right r) = Just r

buildChannelFilename :: FilePath -> Text -> FilePath
buildChannelFilename path name = path </> (T.unpack name ++ ".dat")

getChannelFromDb :: FilePath -> Text -> IO (Maybe Channel)
getChannelFromDb path name = do
    let filename = buildChannelFilename path name

    withFileLock filename Shared $ \_ -> do
        content <- handleJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                        (\_ -> return Nothing)
                        (Just <$> B.readFile filename)

        return (join (eitherToMaybe . decodeLazy <$> content))

updateChannelInDb :: FilePath -> Text -> Date -> Channel -> IO ()
updateChannelInDb path name date newChannel = do
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
    mergeChannel Nothing = setChannelArticleDate date
    mergeChannel (Just oldChannel) = setChannelArticleDate date . pruneOldChannelArticles maxChannelSize . mergeChannelArticles oldChannel
