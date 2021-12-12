-- | Functions for manipulating Pandoc AST.
module EmaWiki.Pandoc
  ( extractTitle
  , rewriteLinks
  , plainify
  ) where

import Relude
import qualified Ema.Helper.Markdown as Markdown
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W


extractTitle :: Pandoc -> (Maybe B.Inlines, Pandoc)
extractTitle (Pandoc meta body) =
  let (h1, body') = extractInitH1 body
      mt = if isJust h1 then h1 else notNullTitle meta
  in (mt, Pandoc meta body')

extractInitH1 :: [B.Block] -> (Maybe B.Inlines, [B.Block])
extractInitH1 (B.Header 1 _ is : rest) =
  if null is then (Nothing, rest) else (Just $ B.fromList is, rest)
extractInitH1 bs = (Nothing, bs)

notNullTitle :: B.Meta -> Maybe B.Inlines
notNullTitle m =
  let t = B.fromList $ B.docTitle m
  in if null t then Nothing else Just t


plainify :: B.Inlines -> Text
plainify = Markdown.plainify . B.toList

-- ------------------------
-- Pandoc transformer
-- ------------------------

rewriteLinks :: (Text -> Text) -> Pandoc -> Pandoc
rewriteLinks f =
  W.walk $ \case
    B.Link attr is (url, title) ->
      B.Link attr is (f url, title)
    x -> x
