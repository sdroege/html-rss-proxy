{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Utils
    ( deduplicateArticles
    , pruneOldChannelArticles
    , mergeChannelArticles
    , setChannelArticleDate
    , dateToText
    , withAttribute
    , ParsingException(..)
    )
where

import Types

import Control.Monad
import Control.Monad.Catch

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nubBy)
import Data.Maybe (fromMaybe)
import Data.Typeable

import Data.Function (on)

import qualified Control.Lens as L
import qualified Text.Xml.Lens as XL

withAttribute :: Applicative f => XL.Name -> Text -> (XL.Element -> f XL.Element) -> XL.Element -> f XL.Element
withAttribute k v = XL.attributed (L.ix k . L.only v)

data ParsingException = ParsingException
    deriving (Typeable, Show)

instance Exception ParsingException

deduplicateArticles :: [Article] -> [Article]
deduplicateArticles = nubBy ((==) `on` articleLink)

pruneOldChannelArticles :: Int -> Channel -> Channel
pruneOldChannelArticles maxChannelSize channel = channel { channelArticles = take maxChannelSize (channelArticles channel) }

mergeChannelArticles :: Channel -> Channel -> Channel
mergeChannelArticles old new = new { channelArticles = mergedArticles }
  where
    oldArticles = channelArticles old
    newArticles = channelArticles new

    mergedArticles = newArticles' ++ updatedArticles
      where
        (newArticles', updatedArticles) = foldr go (newArticles, []) oldArticles
        -- Check if old article was updated, and return remaining new articles
        -- and the old/updated articles
        go o (ns, us) = (ns', u:us)
          where
            -- New version of old article, if any, and list of new articles without the new version
            (e, ns') = extract (\a -> articleLink a == articleLink o) ns
            -- Updated article with old date if any update, or old article
            u = maybe o (\e' -> e' { articleDate = articleDate o }) e

    extract :: (a -> Bool) -> [a] -> (Maybe a, [a])
    extract p = foldr go (Nothing, [])
      where
        go x (m@(Just _), xs') = (m, x:xs')
        go x (Nothing, xs') | p x = (Just x, xs')
                            | otherwise = (Nothing, x:xs')

setChannelArticleDate :: Date -> Channel -> Channel
setChannelArticleDate date channel = channel { channelArticles = map updateArticleDate (channelArticles channel) }
  where
    updateArticleDate article = article { articleDate = Just (fromMaybe date (articleDate article)) }


dateToText :: Date -> Text
dateToText (Date year month day hour minute second) =
    T.pack (show day) <> " " <>
    monthToName month <> " " <>
    T.pack (show year) <> " " <>
    T.pack (show hour) <> ":" <>
    twoDigits minute <> ":" <>
    twoDigits second <> " " <>
    "GMT"
  where
    monthToName 1 = "Jan"
    monthToName 2 = "Feb"
    monthToName 3 = "Mar"
    monthToName 4 = "Apr"
    monthToName 5 = "May"
    monthToName 6 = "Jun"
    monthToName 7 = "Jul"
    monthToName 8 = "Aug"
    monthToName 9 = "Sep"
    monthToName 10 = "Oct"
    monthToName 11 = "Nov"
    monthToName 12 = "Dec"
    monthToName _ = error "invalid number"

    twoDigits x | x >= 0 && x < 10 = "0" <> T.pack (show x)
                | x < 100          = T.pack (show x)
    twoDigits _ = error "invalid number"

