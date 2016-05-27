{-# LANGUAGE OverloadedStrings #-}

import Types
import Rss
import qualified ToVima
import qualified MakThes
import qualified ThePressProject

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar

import Control.Monad.Reader
import Control.Monad.IO.Class

import Web.Scotty

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import System.Clock

type Channels = Map T.Text Channel

updateInterval :: Integer
updateInterval = 1000000000 * 60 * 15 -- 15 minutes in ns

channelList :: [(String, T.Text, IO Channel)]
channelList =
    [ ("/to-vima", "ToVima", ToVima.getChannel)
    , ("/makthes", "MakThes", MakThes.getChannel)
    , ("/the-press-project", "ThePressProject", ThePressProject.getChannel)
    ]

main :: IO ()
main = do
    channels <- newMVar M.empty
    void $ async (updateChannels channels)
    scotty 8080 $
        forM_ channelList $ \(path, name, _) ->
            get (literal path) (getRss channels name)

getRss :: MVar Channels -> T.Text -> ActionM ()
getRss channels name = do
    setHeader "Content-Type" "application/rss+xml; charset=UTF-8"

    currentChannels <- liftIO (readMVar channels)
    let rssDoc = renderChannelToRSS <$> M.lookup name currentChannels
    case rssDoc of
      Nothing -> raise ("Empty channel " `TL.append` TL.fromStrict name)
      Just doc -> raw doc

updateChannels :: MVar Channels -> IO ()
updateChannels channels = getCurrentTime >>= go
  where
    getCurrentTime = toNanoSecs <$> getTime Monotonic

    go prevTime = do
        -- TODO: exception handling
        -- TODO: Persistent storage for the channels
        forM_ channelList $ \(_, name, getChannel) -> do
            channel <- getChannel
            -- TODO: Merge old and new channel, drop too old items
            --       Remember time first seen if none provided
            --       Update text if changed but not time
            modifyMVar_ channels (return . M.insert name channel)

        currentTime <- getCurrentTime
        let nextTime   = prevTime + updateInterval
            waitTime   = nextTime - currentTime
            waitTimeUs = fromIntegral $ waitTime `div` 1000

        -- Wait until the next step if we're not late
        when (waitTimeUs > 0) $
            liftIO $ threadDelay waitTimeUs

        go nextTime

