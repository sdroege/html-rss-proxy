{-# LANGUAGE OverloadedStrings #-}

import Types
import Rss
import qualified ToVima
import qualified MakThes

import Control.Monad.IO.Class

import Web.Scotty

main :: IO ()
main = scotty 8080 $ do
    get "/to-vima" (getRss ToVima.getChannel)
    get "/makthes" (getRss MakThes.getChannel)

getRss :: IO Channel -> ActionM ()
getRss getChannel = do
    setHeader "Content-Type" "application/rss+xml; charset=UTF-8"

    channel <- liftIO getChannel
    let rssDoc = renderChannelToRSS channel

    raw rssDoc

