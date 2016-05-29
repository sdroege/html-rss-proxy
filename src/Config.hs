{-# LANGUAGE OverloadedStrings #-}

module Config
    ( port
    , updateInterval
    , retryCount
    , maxChannelSize
    , channelList
    )
where

import Types
import qualified ToVima
import qualified MakThes
import qualified ThePressProject

import qualified Data.Text as T

port :: Int
port = 8080

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

