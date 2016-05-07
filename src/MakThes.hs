{-# LANGUAGE OverloadedStrings #-}

module MakThes
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
import qualified Data.Conduit.List as CL
import Network.HTTP.Simple (httpSink)
import Network.HTTP.Conduit (parseUrl)
import qualified Data.XML.Types as XT
import qualified Text.XML.Stream.Parse as XP
import qualified Text.HTML.DOM as HTML

data MakThesArticle = MakThesArticle
    { articleTitle :: Text
    , articleDate :: Text
    , articleText :: Text
    , articleLink :: Text
    } deriving (Show)

-- Parsing of HTML into Articles
urlBase, url :: String
urlBase = "http://www.makthes.gr"
url = "http://www.makthes.gr/ItemsFlow/GR/Roi_Eidiseon"

getChannel :: (MonadIO m) => m Channel
getChannel = do
    articles <- fmap (fmap toArticle) $ do
        req <- liftIO $ parseUrl url
        liftIO $ httpSink req $ \_ ->
            HTML.eventConduit =$= parseHtml

    pure Channel { channelTitle = "ΜακΘεσ"
                 , channelLink = "http://www.makthes.gr"
                 , channelDescription = "Εφημερίδα Μακεδονία της Θεσσαλονίκης"
                 , channelArticles = articles
                 }

-- FIXME: The date without a proper time is not useful
toArticle :: MakThesArticle -> Article
toArticle (MakThesArticle title date text link) = Article title link description Nothing
    where
        description = date `T.append` "<br/><br/>" `T.append` text

-- FIXME: How to make this a Conduit XT.Event m MakThesArticle instead, i.e.
-- produce the articles lazily?
parseHtml :: (MonadThrow m) => ConduitM XT.Event o m [MakThesArticle]
parseHtml = XP.force "html" $
    XP.tagName "html" XP.ignoreAttrs $ \_ -> do
        void (XP.many (XP.ignoreTree (/= "body")))
        articles <- XP.force "body" $ XP.tagName "body" XP.ignoreAttrs $ \_ ->
            mconcat <$> XP.many' parseForm1
        void (XP.many XP.ignoreAllTreesContent)
        pure articles

parseForm1 :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [MakThesArticle])
parseForm1 = tagNameWithAttrValue "form" "id" "form1" $
    mconcat <$> XP.many' parseDivMainPlaceholder

parseDivMainPlaceholder :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [MakThesArticle])
parseDivMainPlaceholder = tagNameWithAttrValue "div" "id" "mainplaceholder" $
    mconcat <$> XP.many' parseDivInnerPage

parseDivInnerPage :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [MakThesArticle])
parseDivInnerPage = tagNameWithAttrValue "div" "id" "innerpage" $
    mconcat <$> XP.many' parseDivMainContet

parseDivMainContet :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [MakThesArticle])
parseDivMainContet = tagNameWithAttrValue "div" "class" "maincontet" $
    mconcat <$> XP.many' parseDivItemsFlow

parseDivItemsFlow :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [MakThesArticle])
parseDivItemsFlow = tagNameWithAttrValue "div" "id" "divitemsflow" $
    XP.many parseItem

parseItem :: (MonadThrow m) => ConduitM XT.Event o m (Maybe MakThesArticle)
parseItem = do
    date <- tagNameWithAttrValue "div" "class" "date red" XP.content
    case date of
      Nothing       -> pure Nothing
      Just realDate -> do

          (title, link) <- XP.force "div class title" $ tagNameWithAttrValue "div" "class" "title" $
              XP.force "a title" $ XP.tagName "a" (XP.requireAttr "href" <* XP.ignoreAttrs) $ \link -> do
                  title <- XP.content
                  pure (title, T.pack urlBase `T.append` link)
          text <- XP.force "text" $ tagNameWithAttrValue "div" "class" "text" XP.content
          void (XP.ignoreTagName "hr")
          pure (Just (MakThesArticle title realDate text link))
