{-# LANGUAGE RankNTypes #-}

module Utils
    ( tagNameWithAttrValue
    , deduplicateArticles
    )
where

import Types

import Control.Monad
import Control.Monad.Catch

import Data.Text (Text)
import Data.List (nubBy)

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

