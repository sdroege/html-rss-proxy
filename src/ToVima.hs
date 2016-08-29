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

import Data.String
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Data.List.Split

import qualified Data.ByteString.Lazy as BL

import Control.Lens
import Text.Xml.Lens

import qualified Network.HTTP.Simple as HTTP

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

getChannel :: (MonadIO m, MonadThrow m) => m Channel
getChannel = do
    resp <- HTTP.httpLBS (fromString urlMain)

    let body = HTTP.getResponseBody resp
    let articlesMain = fmap (fmap toArticle) $ parseHtml body

    articlesNext <- fmap sequenceA $ forM [0, 1] $ \n -> do
        resp <- HTTP.httpLBS (fromString (url n))
        let body = HTTP.getResponseBody resp
        return $ fmap (fmap toArticle) $ parseContainers body

    let articles = (:) <$> articlesMain <*> articlesNext

    case articles of
      Nothing       -> throwM ParsingException
      Just articles -> pure Channel { channelTitle = "Το Βήμα Online"
                                    , channelLink = "http://www.tovima.gr/en"
                                    , channelDescription = "Latest news from Greece in English"
                                    , channelArticles = mconcat articles
                                    }

toArticle :: ToVimaArticle -> Article
toArticle (ToVimaArticle title date smallTitle extraText img link) = Article title (T.pack urlBase <> link) description Nothing
    where
        imgTag = case img of
                   Nothing  -> T.empty
                   Just src -> "<img src=\"" <> src <> "\"/>"

        description = imgTag <> date <> "<br/>" <>
                            if T.null smallTitle
                               then extraText
                               else (smallTitle <>
                                     "<br/><br/>" <>
                                     extraText)

parseHtml :: BL.ByteString -> Maybe [ToVimaArticle]
parseHtml body = case articles of
                   Nothing -> Nothing
                   Just [] -> Nothing
                   Just xs -> Just xs
    where
        -- Extract the list of divs that contain what we're interested in
        extractedArticleDivs =
            body ^.. html ...
                    named (only "body") ...
                        named (only "div") . withAttribute "id" "pagewrap" ...
                            named (only "div") . withAttribute "id" "content" ...
                                filtered (has (named (only "div") . withAttribute "class" "container"))

        -- Convert the groups of divs to our data structure
        articles = sequenceA $ fmap divToArticle  extractedArticleDivs

parseContainers :: BL.ByteString -> Maybe [ToVimaArticle]
parseContainers body = case articles of
                   Nothing -> Nothing
                   Just [] -> Nothing
                   Just xs -> Just xs
    where
        -- Extract the list of divs that contain what we're interested in
        extractedArticleDivs =
            body ^.. html ...
                filtered (has (named (only "div") . withAttribute "class" "container"))

        -- Convert the groups of divs to our data structure
        articles = sequenceA $ fmap divToArticle  extractedArticleDivs

divToArticle :: Element -> Maybe ToVimaArticle
divToArticle divArticle = ToVimaArticle <$> bigTitle <*> date <*> smallTitle <*> extraText <*> img <*> link
    where
        linkA = divArticle ^? plate .
            named (only "a")

        link = linkA ^? traverse . attr "href" . _Just
        img = linkA ^. traverse . ix 0 .
            named (only "div") . withAttribute "class" "foto" ...
                named (only "img") . attr "src" & Just

        divText = divArticle ^? plate .
            named (only "div") . withAttribute "class" "text"

        bigTitle = divText ^? traverse ...
            named (only "h3") ...
                named (only "span") . withAttribute "class" "big_title" ...
                    named (only "a") . text & fmap T.strip

        smallTitle = divText ^? traverse ...
            named (only "span") . withAttribute "class" "small_title" . text & fmap T.strip

        date = divText ^? traverse ...
            named (only "span") . withAttribute "class" "date" . text & fmap T.strip

        extraText = divArticle ^? plate .
            named (only "span") . withAttribute "class" "extra_text" . text & fmap T.strip

