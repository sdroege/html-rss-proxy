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

import Data.String
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Data.List.Split

import qualified Data.ByteString.Lazy as BL

import Control.Lens
import Text.Xml.Lens

import qualified Network.HTTP.Simple as HTTP

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

getChannel :: (MonadIO m, MonadThrow m) => m Channel
getChannel = do
    resp <- HTTP.httpLBS (fromString url)
    let body = HTTP.getResponseBody resp
        articles = fmap (fmap toArticle) (parseHtml body)

    case articles of
      Nothing       -> throwM ParsingException
      Just articles -> pure Channel { channelTitle = "ΜακΘεσ"
                                    , channelLink = "http://www.makthes.gr"
                                    , channelDescription = "Εφημερίδα Μακεδονία της Θεσσαλονίκης"
                                    , channelArticles = articles
                                    }

-- FIXME: The date without a proper time is not useful
toArticle :: MakThesArticle -> Article
toArticle (MakThesArticle title date summary link) = Article title (T.pack urlBase <> link) description Nothing
    where
        description = date <> "<br/><br/>" <> summary

parseHtml :: BL.ByteString -> Maybe [MakThesArticle]
parseHtml body = case articles of
                   Nothing -> Nothing
                   Just [] -> Nothing
                   Just xs -> Just xs
    where
        -- Extract the list of divs that contain what we're interested in
        extractedArticleDivs =
            body ^.. html ...
                    named (only "body") ...
                        named (only "form") . withAttribute "id" "form1" ...
                            named (only "div") . withAttribute "id" "mainplaceholder" ...
                                named (only "div") . withAttribute "id" "innerpage" ...
                                    named (only "div") . withAttribute "class" "maincontet" ...
                                        named (only "div") . withAttribute "id" "divitemsflow" .
                                            plate

        -- Group them, each article is 3 divs followed by an hr
        groupedArticleDivs = (split . dropFinalBlank . dropDelims . whenElt) (\a -> a ^. name == "hr") extractedArticleDivs

        -- Convert the groups of divs to our data structure
        articles = sequenceA $ fmap divGroupToArticle groupedArticleDivs

divGroupToArticle :: [Element] -> Maybe MakThesArticle
divGroupToArticle [dateRedDiv, divTitle, divText] = MakThesArticle <$> title <*> date <*> summary <*> link
    where
        date = dateRedDiv ^? withAttribute "class" "date red" . text
        titleLinkA = divTitle ^? withAttribute "class" "title" ...
                        named (only "a")
        link = titleLinkA ^? _Just . attr "href" . _Just
        title = titleLinkA ^? _Just . text
        summary = Just $ divText ^. withAttribute "class" "text" . text

divGroupToArticle _ = Nothing

