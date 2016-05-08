{-# LANGUAGE OverloadedStrings #-}

module ThePressProject
    ( getChannel
    )
where

import Types
import Utils

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Text (Text)
import qualified Data.Text as T

import Data.Conduit
import Network.HTTP.Simple (httpSink)
import Network.HTTP.Conduit (parseUrl)
import qualified Data.XML.Types as XT
import qualified Text.XML.Stream.Parse as XP
import qualified Text.HTML.DOM as HTML

data ThePressProjectArticle = ThePressProjectArticle
    { articleTitle :: Text
    , articleDate :: Text
    , articleText :: Text
    , articleLink :: Text
    , articleImage :: Text
    } deriving (Show)

-- Parsing of HTML into Articles
urlBase, url :: String
urlBase = "http://www.thepressproject.gr"
url = "http://www.thepressproject.gr/list_en.php"

getChannel :: (MonadIO m) => m Channel
getChannel = do
    articles <- fmap (fmap toArticle) $ do
        req <- liftIO $ parseUrl url
        liftIO $ httpSink req $ \_ ->
            HTML.eventConduit =$= parseHtml

    pure Channel { channelTitle = "ThePressProject"
                 , channelLink = "http://www.thepressproject.gr"
                 , channelDescription = "ThePressProject"
                 , channelArticles = articles
                 }

-- FIXME: The date without a proper time is not useful
toArticle :: ThePressProjectArticle -> Article
toArticle (ThePressProjectArticle title date text link img) = Article title link description Nothing
    where
        [day, month, year] = T.splitOn "/" date
        realDate = day `T.append` "." `T.append` month `T.append` ".20" `T.append` year
        description = "<img src=\"" `T.append` img `T.append` "\"/>" `T.append` realDate `T.append` "<br/><br/>" `T.append` text

-- FIXME: How to make this a Conduit XT.Event m ThePressProjectArticle instead, i.e.
-- produce the articles lazily?
parseHtml :: (MonadThrow m) => ConduitM XT.Event o m [ThePressProjectArticle]
parseHtml = XP.force "html" $
    XP.tagName "html" XP.ignoreAttrs $ \_ -> do
        void (XP.many (XP.ignoreTree (/= "body")))
        articles <- XP.force "body" $ XP.tagName "body" XP.ignoreAttrs $ \_ ->
            mconcat <$> XP.many' parseDivFull
        void (XP.many XP.ignoreAllTreesContent)
        pure articles

parseDivFull :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [ThePressProjectArticle])
parseDivFull = tagNameWithAttrValue "div" "class" "full" $
    mconcat <$> XP.many' parseDivMainLeft

parseDivMainLeft :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [ThePressProjectArticle])
parseDivMainLeft = tagNameWithAttrValue "div" "class" "mainleft" $
    mconcat <$> XP.many' parseDivC70

parseDivC70 :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [ThePressProjectArticle])
parseDivC70 = tagNameWithAttrValue "div" "class" "c70" $
    XP.force "div inner c70" $ XP.tagName "div" XP.ignoreAttrs $ \_ ->
        mconcat <$> XP.many' parseDivSemiRight

parseDivSemiRight :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [ThePressProjectArticle])
parseDivSemiRight = tagNameWithAttrValue "div" "class" "semiright" $
    mconcat <$> XP.many' parseDivList

parseDivList :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [ThePressProjectArticle])
parseDivList = tagNameWithAttrValue "div" "class" "list" $
    XP.many parseItem

parseItem :: (MonadThrow m) => ConduitM XT.Event o m (Maybe ThePressProjectArticle)
parseItem = do
    article <- XP.tagName "div" XP.ignoreAttrs $ \_ -> do
        (date, img, link) <- parseDateImgLink
        (title, text) <- parseText

        pure (ThePressProjectArticle title date text link img)

    case article of
      Nothing -> pure Nothing
      Just _  -> do
          XP.force "div clear" $ XP.tagName "div" XP.ignoreAttrs $ \_ ->
              void (XP.many XP.ignoreAllTreesContent)
          pure article

parseDateImgLink :: (MonadThrow m) => ConduitM XT.Event o m (Text, Text, Text)
parseDateImgLink = XP.force "div date" $ XP.tagName "div" XP.ignoreAttrs $ \_ -> do
    (link, img) <- XP.force "div line" $ XP.tagName "div" XP.ignoreAttrs $ \_ ->
        XP.force "a" $ XP.tagName "a" (XP.requireAttr "href" <* XP.ignoreAttrs) $ \href -> do
            img <- XP.force "img" $ XP.tagName "img" (XP.requireAttr "src" <* XP.ignoreAttrs) pure
            pure (T.pack urlBase `T.append` "/" `T.append` href, T.pack urlBase `T.append` T.drop 2 img)

    date <- XP.force "div" $ XP.tagName "div" XP.ignoreAttrs $ \_ ->
        XP.force "div open" $ tagNameWithAttrValue "div" "class" "open" $
            XP.force "table" $ XP.tagName "table" XP.ignoreAttrs $ \_ ->
                XP.force "tr" $ XP.tagName "tr" XP.ignoreAttrs $ \_ ->
                    XP.force "td" $ XP.tagName "td" XP.ignoreAttrs $ const XP.content

    pure (date, img, link)

parseText :: (MonadThrow m) => ConduitM XT.Event o m (Text, Text)
parseText = XP.force "div text" $ XP.tagName "div" XP.ignoreAttrs $ \_ -> do
    title <- XP.force "div title link" $ XP.tagName "div" XP.ignoreAttrs $ \_ ->
        XP.force "a title" $ XP.tagName "a" (XP.requireAttr "href" <* XP.ignoreAttrs) $ const XP.content

    XP.force "style" $ XP.tagName "style" XP.ignoreAttrs $ \_ ->
        void (XP.many XP.ignoreAllTreesContent)

    XP.force "div gap" $ XP.tagName "div" XP.ignoreAttrs $ \_ ->
        void (XP.many XP.ignoreAllTreesContent)

    text <- XP.force "div text" $ XP.tagName "div" XP.ignoreAttrs $ \_ ->
        XP.force "a text" $ XP.tagName "a" XP.ignoreAttrs $ \_ -> do
            void $ tagNameWithAttrValue "div" "class" "ticker" $
                void XP.ignoreAllTreesContent
            XP.content

    XP.force "div open" $ tagNameWithAttrValue "div" "class" "open" $
        void (XP.many XP.ignoreAllTreesContent)

    pure (title, text)

