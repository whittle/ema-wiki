{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

-- As such it might be a little too involved. Simpler examples can be found here,
--   https://github.com/srid/ema/tree/master/src/Ema/Example
module Main where

import Relude
import Control.Exception (throw)
import Control.Monad.Logger
import Data.LVar (LVar)
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as FileSystem
import qualified EmaWiki.Config as Config
import qualified EmaWiki.Model as Model
import qualified EmaWiki.Render as Render
import qualified Env
import UnliftIO (MonadUnliftIO)


-- ------------------------
-- Main entry point
-- ------------------------

log :: MonadLogger m => Text -> m ()
log = logInfoNS "ema-wiki"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "ema-wiki"

main :: IO ()
main = do
  -- Gets the subcommand the executable was run with
  cli <- Ema.CLI.cliAction

  -- Needs to know the subcommand used
  conf <- getConfig $ Ema.CLI.action cli

  -- Start the dev server or generate static site if `gen` argument is passed.
  -- It is designed to work well with ghcid (which is what the bin/run script
  -- uses).
  Ema.runEmaWithCli cli (Render.render conf) modelingThread


-- | Fetches the config from env vars. Aborts with a friendly message if
-- `Config.Config` cannot be fully parsed.
getConfig :: Ema.CLI.Action -> IO Config.Config
getConfig a = Env.parse header $ Config.configParser a
  where header = Env.header "ema-wiki 0.1.0.0"


-- | Initializes a `Model.Model` and updates it every time the watched directory
-- changes. Intended to be run in a (long-running) thread of its own. Does not
-- generate any HTML or other output itself.
modelingThread :: (MonadLogger m, MonadUnliftIO m)
               => Ema.CLI.Action
               -> LVar Model.Model
               -> m ()
modelingThread _act model = do
  -- Use the FileSystem helper to directly "mount" our files on to the LVar.
  let pats = [((), "*.md")]
      ignorePats = [".*"]
  void $ FileSystem.mountOnLVar "." pats ignorePats model Model.initModel $ const modelTransform


-- | Responds to a filesystem change with the appropriate transform to the
-- `Model.Model`.
modelTransform :: (MonadIO m, MonadLogger m)
               => FilePath
               -> FileSystem.FileAction ()
               -> m (Model.Model -> Model.Model)
modelTransform fp = \case
  FileSystem.Refresh _ () ->
    maybe id (uncurry Model.modelInsert) <$> readSource fp
  FileSystem.Delete ->
    pure $ maybe id Model.modelDelete $ Model.mkMarkdownRoute fp


-- | Reads a Markdown file and parses it into a `Model.Doc`. May throw
-- `BadMarkdown` if unable to parse the file.
readSource :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe (Model.MarkdownRoute, Model.Doc))
readSource fp =
  runMaybeT $ do
    r :: Model.MarkdownRoute <- MaybeT $ pure $ Model.mkMarkdownRoute fp
    logD $ "Reading " <> toText fp
    s <- readFileText fp
    let d = either (throw . BadMarkdown) id $ Model.parseDoc r fp s
    pure (r, d)


newtype BadMarkdown = BadMarkdown Text
  deriving (Show, Exception)
