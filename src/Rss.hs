{-# LANGUAGE OverloadedStrings #-}

module Rss
    ( renderChannelToRSS
    )
where

import Types

import Control.Monad

import Data.Default (def)

import Data.ByteString.Lazy (ByteString)

import qualified Text.XML as X
import qualified Text.XML.Writer as XW

import qualified Data.Map.Lazy as M

renderChannelToRSS :: Channel -> ByteString
renderChannelToRSS = X.renderLBS def { X.rsPretty = True } . generateRss

generateRss :: Channel -> X.Document
generateRss (Channel cTitle cLink cDescription articles) =
    let doc =
            XW.document "rss" $
                XW.element "channel" $ do
                    XW.element "title" cTitle
                    XW.element "link" cLink
                    XW.element "description" cDescription
                    forM_ articles $ \(Article title link description) ->
                        XW.element "item" $ do
                            XW.element "title" title
                            XW.element "link" link
                            XW.element "description" description
                            XW.elementA "guid" [(X.Name "isPermaLink" Nothing Nothing, "true")] link
    in
        doc { X.documentRoot = (X.documentRoot doc) { X.elementAttributes = M.singleton (X.Name "version" Nothing Nothing) "2.0" } }
