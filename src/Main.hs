{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Types
import Rss
import Utils
import qualified ToVima
import qualified MakThes
import qualified ThePressProject

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar

import Control.Monad.Reader
import Control.Monad.IO.Class

import Control.Exception
import System.Exit

import Web.Scotty

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.XML.Stream.Parse (XmlException)

import System.Clock
import Data.Time.Clock
import Data.Time.Calendar

type Channels = Map T.Text Channel
type ErrorChannels = Map T.Text (Either T.Text Int)

updateInterval :: Integer
updateInterval = 1000000000 * 60 * 15 -- 15 minutes in ns

retryCount :: Int
retryCount = 3

maxChannelSize :: Int
maxChannelSize = 50

channelList :: [(String, T.Text, IO Channel)]
channelList =
    [ ("/to-vima", "ToVima", ToVima.getChannel)
    , ("/makthes", "MakThes", MakThes.getChannel)
    , ("/the-press-project", "ThePressProject", ThePressProject.getChannel)
    ]

main :: IO ()
main = do
    channels <- newMVar (M.empty, M.empty)
    void $ async (updateChannels channels)
    scotty 8080 $
        forM_ channelList $ \(path, name, _) ->
            get (literal path) (getRss channels name)

getRss :: MVar (Channels, ErrorChannels) -> T.Text -> ActionM ()
getRss channels name = do
    setHeader "Content-Type" "application/rss+xml; charset=UTF-8"

    (currentChannels, currentErrorChannels) <- liftIO (readMVar channels)

    case M.lookup name currentErrorChannels of
      Just (Left err) -> raise ("Error in channel " `TL.append` TL.fromStrict name `TL.append` TL.fromStrict err)
      _               -> do
          let rssDoc = renderChannelToRSS <$> M.lookup name currentChannels
          case rssDoc of
            Nothing -> raise ("Empty channel " `TL.append` TL.fromStrict name)
            Just doc -> raw doc

updateChannels :: MVar (Channels, ErrorChannels) -> IO ()
updateChannels channels = getCurrentMonotonicTime >>= go
  where
    getCurrentMonotonicTime = toNanoSecs <$> getTime Monotonic

    go prevTime = do
        -- TODO: Persistent storage for the channels
        forM_ channelList $ \(_, name, getChannel) ->
            updateChannel name getChannel

        currentTime <- getCurrentMonotonicTime
        let nextTime   = prevTime + updateInterval
            waitTime   = nextTime - currentTime
            waitTimeUs = fromIntegral $ waitTime `div` 1000

        -- Wait until the next step if we're not late
        when (waitTimeUs > 0) $
            liftIO $ threadDelay waitTimeUs

        go nextTime

    updateChannel name getChannel = flip catches (handlers name) $ do
        channel <- getChannel
        if null (channelArticles channel) then
            storeException name "Empty channel"
            else
                modifyMVar_ channels $ \(currentChannels, currentErrorChannels) -> do
                    (UTCTime nowDay nowTime) <- getCurrentTime
                    let (year, month, day) = toGregorian nowDay
                        seconds = fromInteger (diffTimeToPicoseconds nowTime `div` 1000000000000)
                        (hours, seconds') = divMod seconds (60*60)
                        (minutes, seconds'') = divMod seconds' 60
                        date = Date (fromInteger year) month day hours minutes seconds''
                        newChannels = M.alter (mergeChannel date channel) name currentChannels
                    return (newChannels, M.delete name currentErrorChannels)

    mergeChannel date newChannel Nothing = Just (setChannelArticleDate date newChannel)
    mergeChannel date newChannel (Just oldChannel) = Just mergedChannel
      where
        mergedChannel = setChannelArticleDate date . pruneOldChannelArticles maxChannelSize . mergeChannelArticles oldChannel $ newChannel

    handlers name = [ Handler (\(e :: IOException) -> storeException name (T.pack (show e)))
                    , Handler (\(e :: XmlException) -> storeException name (T.pack (show e)))
                    , Handler (\(e :: SomeException) -> putStrLn ("Exception while updating " ++ T.unpack name ++ ": " ++ show e) >> exitFailure)
                    ]

    storeException name exception =
        modifyMVar_ channels $ \(currentChannels, currentErrorChannels) -> do
            let newErrorChannels =
                    case M.lookup name currentErrorChannels of
                      Just (Left _) -> M.insert name (Left exception) currentErrorChannels
                      Just (Right count) -> if count >= retryCount then
                                              M.insert name (Left exception) currentErrorChannels
                                            else
                                              M.insert name (Right (succ count)) currentErrorChannels
                      Nothing -> M.insert name (Right 1) currentErrorChannels

            return (currentChannels, newErrorChannels)

