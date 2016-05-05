{-# LANGUAGE OverloadedStrings #-}

module ToVima
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

data ToVimaArticle = ToVimaArticle
    { articleTitle :: Text
    , articleSmallTitle :: Text
    , articleExtraText :: Text
    , articleImg :: Maybe Text
    , articleLink :: Text
    } deriving (Show)

-- Parsing of HTML into Articles
urlBase :: String
urlBase = "http://www.tovima.gr"

url :: Int -> String
url page = "http://www.tovima.gr/ajax.aspx?m=Dol.ToVima.ExtenderModule.ModuleUserControl&controlname=/User_Controls/International/ArticleListInternational.ascx&data=page%3D" ++ show page ++ "%26lang%3D1"

getChannel :: (MonadIO m) => m Channel
getChannel = do

    articles <- fmap (deduplicateArticles . concat) $
        forM [0, 1] $ \n -> do
            req <- liftIO $ parseUrl (url n)
            liftIO $ httpSink req $ \_ ->
                HTML.eventConduit =$= parseContainers =$= CL.map toArticle =$= CL.consume

    return Channel { channelTitle = "Το Βήμα Online"
                   , channelLink = "http://www.tovima.gr/en"
                   , channelDescription = "Latest news from Greece in English"
                   , channelArticles = articles
                   }

toArticle :: ToVimaArticle -> Article
toArticle (ToVimaArticle title smallTitle extraText img link) = Article title link description
    where
        imgTag = case img of
                   Nothing  -> T.empty
                   Just src -> "<img src=\"" `T.append` src `T.append` "\"/>"

        description = imgTag `T.append`
                            if T.null smallTitle
                               then extraText
                               else (smallTitle `T.append`
                                     "<br/><br/>" `T.append`
                                     extraText)

parseContainers :: (MonadThrow m) => Conduit XT.Event m ToVimaArticle
parseContainers = XP.manyYield parseContainer

parseContainer :: (MonadThrow m) => ConduitM XT.Event o m (Maybe ToVimaArticle)
parseContainer =
    tagNameWithAttrValue "div" "class" "container" $ do
        (href, img) <- parseLinkImage
        (title, smallTitle) <- parseDivText
        void (XP.ignoreTagName "div")
        extraText <- parseExtraText
        pure (ToVimaArticle title smallTitle extraText img href)

parseLinkImage :: (MonadThrow m) => ConduitM XT.Event o m (Text, Maybe Text)
parseLinkImage = XP.force "link image" $
    XP.tagName "a" (XP.requireAttr "href" <* XP.ignoreAttrs) $ \href -> do
        img <- tagNameWithAttrValue "div" "class" "foto" $
            XP.force "img" $ XP.tagName "img" (XP.requireAttr "src" <* XP.ignoreAttrs) pure
        pure (T.pack urlBase `T.append` href, img)

parseDivText :: (MonadThrow m) => ConduitM XT.Event o m (Text, Text)
parseDivText = XP.force "div text" $
    tagNameWithAttrValue "div" "class" "text" $ do
        void (XP.ignoreTagName "span")
        title <- parseBigTitle
        smallTitle <- parseSmallTitle
        -- TODO: parse span class date
        void (XP.ignoreTreeName "span")
        pure (T.strip title, T.strip smallTitle)

parseBigTitle :: (MonadThrow m) => ConduitM XT.Event o m Text
parseBigTitle = XP.force "h3" $
    XP.tagName "h3" XP.ignoreAttrs $ \_ ->
        XP.force "span big_title" $ tagNameWithAttrValue "span" "class" "big_title" $
            XP.force "a big title" $ XP.tagName "a" XP.ignoreAttrs $ const XP.content

parseSmallTitle :: (MonadThrow m) => ConduitM XT.Event o m Text
parseSmallTitle = XP.force "span small_title" $
    tagNameWithAttrValue "span" "class" "small_title" XP.content

parseExtraText :: (MonadThrow m) => ConduitM XT.Event o m Text
parseExtraText = XP.force "span extra_text" $
    tagNameWithAttrValue "span" "class" "extra_text" (T.strip <$> XP.content)

