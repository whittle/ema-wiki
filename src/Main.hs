{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

-- As such it might be a little too involved. Simpler examples can be found here,
--   https://github.com/srid/ema/tree/master/src/Ema/Example
module Main where

import Control.Exception (throw)
import Control.Monad.Logger
import Data.Default (Default (..))
import qualified Ema
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Markdown as Markdown
import qualified EmaWiki.Model as Model
import qualified EmaWiki.Render as Render
import Text.Pandoc.Definition (Pandoc (..))

-- ------------------------
-- Main entry point
-- ------------------------

log :: MonadLogger m => Text -> m ()
log = logInfoNS "ema-wiki"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "ema-wiki"

main :: IO ()
main =
  -- runEma handles the CLI and starts the dev server (or generate static site
  -- if `gen` argument is passed).  It is designed to work well with ghcid
  -- (which is what the bin/run script uses).
  Ema.runEma Render.render $ \_act model -> do
    -- This is the place where we can load and continue to modify our "model".
    -- You will use `LVar.set` and `LVar.modify` to modify the model.
    --
    -- It is a run in a (long-running) thread of its own.
    --
    -- We use the FileSystem helper to directly "mount" our files on to the
    -- LVar.
    let pats = [((), "**/*.md")]
        ignorePats = [".*"]
    void . FileSystem.mountOnLVar "." pats ignorePats model def $ \() fp action -> do
      case action of
        FileSystem.Refresh _ () -> do
          mData <- readSource fp
          pure $ maybe id (uncurry Model.modelInsert) mData
        FileSystem.Delete ->
          pure $ maybe id Model.modelDelete $ Model.mkMarkdownRoute fp
  where
    readSource :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe (Model.MarkdownRoute, (Model.Meta, Pandoc)))
    readSource fp =
      runMaybeT $ do
        r :: Model.MarkdownRoute <- MaybeT $ pure $ Model.mkMarkdownRoute fp
        logD $ "Reading " <> toText fp
        s <- readFileText fp
        pure
          ( r,
            either (throw . BadMarkdown) (first $ fromMaybe def) $
              Markdown.parseMarkdownWithFrontMatter @Model.Meta Markdown.fullMarkdownSpec fp s
          )

newtype BadMarkdown = BadMarkdown Text
  deriving (Show, Exception)
