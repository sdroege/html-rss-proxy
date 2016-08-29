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

import Data.String
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Data.List.Split

import qualified Data.ByteString.Lazy as BL

import Control.Lens
import Text.Xml.Lens

import qualified Network.HTTP.Simple as HTTP

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

getChannel :: (MonadIO m, MonadThrow m) => m Channel
getChannel = do
    resp <- HTTP.httpLBS (fromString url)
    let body = HTTP.getResponseBody resp
        articles = fmap (fmap toArticle) (parseHtml body)

    case articles of
      Nothing       -> throwM ParsingException
      Just articles -> pure Channel { channelTitle = "ThePressProject"
                                    , channelLink = "http://www.thepressproject.gr"
                                    , channelDescription = "ThePressProject"
                                    , channelArticles = articles
                                    }


-- FIXME: The date without a proper time is not useful
toArticle :: ThePressProjectArticle -> Article
toArticle (ThePressProjectArticle title date summary link img) = Article title (T.pack urlBase <> "/" <> link) description Nothing
    where
        description = "<img src=\"" <> T.pack urlBase <> T.drop 2 img <> "\"/>" <> date <> "<br/><br/>" <> summary


parseHtml :: BL.ByteString -> Maybe [ThePressProjectArticle]
parseHtml body = case articles of
                   Nothing -> Nothing
                   Just [] -> Nothing
                   Just xs -> Just xs
    where
        -- Extract the list of divs that contain what we're interested in
        extractedArticleDivs =
            body ^.. html ...
                    named (only "body") ...
                        named (only "div") . withAttribute "class" "full" ...
                            named (only "div") . withAttribute "class" "mainleft" ...
                                named (only "div") . withAttribute "class" "c70" ...
                                    named (only "div") ...
                                        named (only "div") . withAttribute "class" "semiright" ...
                                            named (only "div") . withAttribute "class" "list" ...
                                                filtered (\e -> has (named (only "div")) e && hasn't (withAttribute "style" "clear:both;") e)

        -- Convert the groups of divs to our data structure
        articles = sequenceA $ fmap divToArticle extractedArticleDivs

divToArticle :: Element -> Maybe ThePressProjectArticle
divToArticle divArticle = ThePressProjectArticle <$> title <*> date <*> summary <*> link <*> img
    where
        leftDiv = divArticle ^? plate .
            named (only "div") . withAttribute "style" "float:left; width:25%;"

        imgA = leftDiv ^? traverse . ix 0 .
            named (only "div") ...
                named (only "a") . filtered (has (node "img"))

        link = imgA ^? traverse . attr "href" . _Just
        img = imgA ^? traverse . ix 0 .
            named (only "img") . attr "src" . _Just

        date = leftDiv ^? traverse . ix 1 .
            named (only "div") ...
                named (only "div") . withAttribute "class" "open" ...
                    named (only "table") ...
                        named (only "tr") ...
                            named (only "td") . text

        rightDiv = divArticle ^? plate .
            named (only "div") . withAttribute "style" "float:left; width:70%;"

        title = rightDiv ^? traverse . ix 0 ...
            named (only "a") . text

        summary = rightDiv ^? traverse . ix 3 ...
            named (only "a") . text

