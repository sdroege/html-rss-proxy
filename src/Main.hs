{-# LANGUAGE OverloadedStrings #-}

import Types
import Rss
import qualified ToVima

import Control.Monad.IO.Class

import Web.Scotty

main :: IO ()
main = scotty 8080 $
    get "/to-vima" (getRss ToVima.getChannel)

getRss :: IO Channel -> ActionM ()
getRss getChannel = do
    setHeader "Content-Type" "application/rss+xml; charset=UTF-8"

    channel <- liftIO getChannel
    let rssDoc = renderChannelToRSS channel

    raw rssDoc

