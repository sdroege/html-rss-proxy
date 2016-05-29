{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Utils
    ( tagNameWithAttrValue
    , deduplicateArticles
    , pruneOldChannelArticles
    , mergeChannelArticles
    , setChannelArticleDate
    , dateToText
    )
where

import Types

import Control.Monad
import Control.Monad.Catch

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nubBy)
import Data.Maybe (fromMaybe)

import Data.Conduit
import Data.Function (on)

import qualified Data.XML.Types as XT
import qualified Text.XML.Stream.Parse as XP

-- Helper function like tagName that also requires a specific attribute with a specific value
tagNameWithAttrValue :: (MonadThrow m) => XT.Name -> XT.Name -> Text -> ConduitM XT.Event o m a -> ConduitM XT.Event o m (Maybe a)
tagNameWithAttrValue n a v p = fmap join $
    XP.tagName n (XP.attr a <* XP.ignoreAttrs) $ \i ->
        case i of
          Just v' | v == v' -> Just <$> p
          _                 -> XP.many XP.ignoreAllTreesContent *> pure Nothing

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
        go o (n, us) = (n', u:us)
          where
            -- New version of old article, if any, and list of new articles without the new version
            (e, n') = extract (\a -> articleLink a == articleLink o) n
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
    T.pack (show day) `T.append` " " `T.append`
    monthToName month `T.append` " " `T.append`
    T.pack (show year) `T.append` " " `T.append`
    T.pack (show hour) `T.append` ":" `T.append`
    twoDigits minute `T.append` ":" `T.append`
    twoDigits second `T.append` " " `T.append`
    "GMT"

monthToName :: Int -> Text
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

twoDigits :: Int -> Text
twoDigits x | x >= 0 && x < 10 = "0" `T.append` T.pack (show x)
            | x < 100          = T.pack (show x)
twoDigits _ = error "invalid number"
