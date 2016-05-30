{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Types
import Rss
import Db
import Config

import Data.Monoid

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

import Data.Acid
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)

import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataDir)

type ErrorChannels = Map T.Text (Either T.Text Int)

main :: IO ()
main = do
    location <- getUserDataDir "html-rss-proxy"
    createDirectoryIfMissing True location

    bracket (openLocalStateFrom location (Channels M.empty)) createCheckpointAndClose $ \acid -> do
        createArchive acid
        errorChannels <- newMVar M.empty
        void $ async (updateChannels acid errorChannels)
        scotty port $
            forM_ channelList $ \(path, name, _) ->
                get (literal path) (getRss acid errorChannels name)

getRss :: AcidState Channels -> MVar ErrorChannels -> T.Text -> ActionM ()
getRss acid errorChannels name = do
    setHeader "Content-Type" "application/rss+xml; charset=UTF-8"

    currentErrorChannels <- liftIO (readMVar errorChannels)

    case M.lookup name currentErrorChannels of
      Just (Left err) -> raise ("Error in channel " <> TL.fromStrict name <> TL.fromStrict err)
      _               -> do
          rssDoc <- query' acid (GetChannel name)
          case rssDoc of
            Nothing -> raise ("Empty channel " <> TL.fromStrict name)
            Just doc -> raw (renderChannelToRSS doc)

updateChannels :: AcidState Channels -> MVar ErrorChannels -> IO ()
updateChannels acid errorChannels = getCurrentMonotonicTime >>= go
  where
    getCurrentMonotonicTime = toNanoSecs <$> getTime Monotonic

    go prevTime = do
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
            modifyMVar_ errorChannels $ \currentErrorChannels -> do
                (UTCTime nowDay nowTime) <- getCurrentTime
                let (year, month, day) = toGregorian nowDay
                    seconds = fromInteger (diffTimeToPicoseconds nowTime `div` 1000000000000)
                    (hours, seconds') = divMod seconds (60*60)
                    (minutes, seconds'') = divMod seconds' 60
                    date = Date (fromInteger year) month day hours minutes seconds''
                update' acid (UpdateChannel name date channel)
                return (M.delete name currentErrorChannels)

    handlers name = [ Handler (\(e :: IOException) -> storeException name (T.pack (show e)))
                    , Handler (\(e :: XmlException) -> storeException name (T.pack (show e)))
                    , Handler (\(e :: SomeException) -> putStrLn ("Exception while updating " ++ T.unpack name ++ ": " ++ show e) >> exitFailure)
                    ]

    storeException name exception =
        modifyMVar_ errorChannels $ \currentErrorChannels -> do
            let newErrorChannels =
                    case M.lookup name currentErrorChannels of
                      Just (Left _) -> M.insert name (Left exception) currentErrorChannels
                      Just (Right count) -> if count >= retryCount then
                                              M.insert name (Left exception) currentErrorChannels
                                            else
                                              M.insert name (Right (succ count)) currentErrorChannels
                      Nothing -> M.insert name (Right 1) currentErrorChannels

            return newErrorChannels

