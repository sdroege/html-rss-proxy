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
    , articleDate :: Text
    , articleSmallTitle :: Text
    , articleExtraText :: Text
    , articleImg :: Maybe Text
    , articleLink :: Text
    } deriving (Show)

-- Parsing of HTML into Articles
urlBase, urlMain :: String
urlBase = "http://www.tovima.gr"
urlMain = "http://www.tovima.gr/en"

url :: Int -> String
url page = "http://www.tovima.gr/ajax.aspx?m=Dol.ToVima.ExtenderModule.ModuleUserControl&controlname=/User_Controls/International/ArticleListInternational.ascx&data=page%3D" ++ show page ++ "%26lang%3D1"

getChannel :: (MonadIO m) => m Channel
getChannel = do
    articles <- fmap (fmap toArticle) $ do
        req <- liftIO $ parseUrl urlMain
        liftIO $ httpSink req $ \_ ->
            HTML.eventConduit =$= parseHtml
    articlesNext <- forM [0, 1] $ \n -> do
            req <- liftIO $ parseUrl (url n)
            liftIO $ httpSink req $ \_ ->
                HTML.eventConduit =$= parseContainers =$= CL.map toArticle =$= CL.consume

    pure Channel { channelTitle = "Το Βήμα Online"
                 , channelLink = "http://www.tovima.gr/en"
                 , channelDescription = "Latest news from Greece in English"
                 , channelArticles = deduplicateArticles . mconcat $ (articles : articlesNext)
                 }

-- FIXME: The date without a proper time is not useful
toArticle :: ToVimaArticle -> Article
toArticle (ToVimaArticle title date smallTitle extraText img link) = Article title link description Nothing --(Just rfc822Date)
    where
        imgTag = case img of
                   Nothing  -> T.empty
                   Just src -> "<img src=\"" `T.append` src `T.append` "\"/>"
        --[_, dayMonth, year] = fmap T.strip (T.splitOn "," date)
        --[month, day] = fmap T.strip (T.splitOn " " dayMonth)
        --rfc822Date = day `T.append` " " `T.append` month `T.append` " " `T.append` year `T.append`
        --                " 00:00:01"

        description = imgTag `T.append` date `T.append` "<br/>" `T.append`
                            if T.null smallTitle
                               then extraText
                               else (smallTitle `T.append`
                                     "<br/><br/>" `T.append`
                                     extraText)

-- FIXME: How to make this a Conduit XT.Event m ToVimaArticle instead, i.e.
-- produce the articles lazily?
parseHtml :: (MonadThrow m) => ConduitM XT.Event o m [ToVimaArticle]
parseHtml = XP.force "html" $
    XP.tagName "html" XP.ignoreAttrs $ \_ -> do
        void (XP.many (XP.ignoreTree (/= "body")))
        articles <- XP.force "body" $ XP.tagName "body" XP.ignoreAttrs $ \_ ->
            mconcat <$> XP.many' parseDivPagewrap
        void (XP.many XP.ignoreAllTreesContent)
        pure articles

parseDivPagewrap :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [ToVimaArticle])
parseDivPagewrap = tagNameWithAttrValue "div" "id" "pagewrap" $
    mconcat <$> XP.many' parseDivContent

parseDivContent :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [ToVimaArticle])
parseDivContent = tagNameWithAttrValue "div" "id" "content" $
    XP.many' parseContainer

parseContainers :: (MonadThrow m) => Conduit XT.Event m ToVimaArticle
parseContainers = XP.manyYield parseContainer

parseContainer :: (MonadThrow m) => ConduitM XT.Event o m (Maybe ToVimaArticle)
parseContainer =
    tagNameWithAttrValue "div" "class" "container" $ do
        (href, img) <- parseLinkImage
        (title, smallTitle, date) <- parseDivText
        void (XP.ignoreTagName "div")
        extraText <- parseExtraText
        pure (ToVimaArticle title date smallTitle extraText img href)

parseLinkImage :: (MonadThrow m) => ConduitM XT.Event o m (Text, Maybe Text)
parseLinkImage = XP.force "link image" $
    XP.tagName "a" (XP.requireAttr "href" <* XP.ignoreAttrs) $ \href -> do
        img <- tagNameWithAttrValue "div" "class" "foto" $
            XP.force "img" $ XP.tagName "img" (XP.requireAttr "src" <* XP.ignoreAttrs) pure
        pure (T.pack urlBase `T.append` href, img)

parseDivText :: (MonadThrow m) => ConduitM XT.Event o m (Text, Text, Text)
parseDivText = XP.force "div text" $
    tagNameWithAttrValue "div" "class" "text" $ do
        void (XP.ignoreTagName "span")
        title <- parseBigTitle
        smallTitle <- parseSmallTitle
        date <- parseDate
        pure (T.strip title, T.strip smallTitle, T.strip date)

parseBigTitle :: (MonadThrow m) => ConduitM XT.Event o m Text
parseBigTitle = XP.force "h3" $
    XP.tagName "h3" XP.ignoreAttrs $ \_ ->
        XP.force "span big_title" $ tagNameWithAttrValue "span" "class" "big_title" $
            XP.force "a big title" $ XP.tagName "a" XP.ignoreAttrs $ const XP.content

parseSmallTitle :: (MonadThrow m) => ConduitM XT.Event o m Text
parseSmallTitle = XP.force "span small_title" $
    tagNameWithAttrValue "span" "class" "small_title" XP.content

parseDate :: (MonadThrow m) => ConduitM XT.Event o m Text
parseDate = XP.force "span date" $
    tagNameWithAttrValue "span" "class" "date" XP.content

parseExtraText :: (MonadThrow m) => ConduitM XT.Event o m Text
parseExtraText = XP.force "span extra_text" $
    tagNameWithAttrValue "span" "class" "extra_text" (T.strip <$> XP.content)

