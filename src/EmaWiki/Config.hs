-- | Configuration settings passed in through env vars.
module EmaWiki.Config
  ( Config(..)
  , configParser
  ) where

import Relude
import qualified Ema.CLI
import Env


data Config = Config
  { algoliaApplicationId :: Text
  , algoliaIndexName :: Text
  , algoliaSearchOnlyApiKey :: Text
  , contentDir :: Text
  , githubEditBranch :: Text
  , githubRepoUrl :: Text
  , wikiRootUrl :: Text
  } deriving (Eq,Show)


-- | This parser is also used to provide an informative message when the
-- environment does not provde all necessary information.
configParser :: (AsEmpty e, AsUnset e)
             => Ema.CLI.Action -> Parser e Config
configParser a = Config
  <$> var (str <=< nonempty)
          "ALGOLIA_APPLICATION_ID"
          (devKeep a <> help "ID for DocSearch app in Algolia")
  <*> var (str <=< nonempty)
          "ALGOLIA_INDEX_NAME"
          (devKeep a <> help "Index name in Algolia")
  <*> var (str <=< nonempty)
          "ALGOLIA_SEARCH_ONLY_API_KEY"
          (devKeep a <> help "Search-only API key for Algolia app")
  <*> var (str <=< nonempty)
          "CONTENT_DIR"
          (devKeep a <> help "Directory path from the base of the repo to the content")
  <*> var (str <=< nonempty)
          "GH_EDIT_BRANCH"
          (devKeep a <> help "Branch to link to for content editing")
  <*> var (str <=< nonempty)
          "GH_REPO_URL"
          (devKeep a <> help "Root URL for GitHub repo where content is stored")
  <*> var (str <=< nonempty)
          "WIKI_ROOT_URL"
          (devKeep a <> help "Root URL the wiki will be hosted when published")

-- | During development, the executable is run repeatedly inside ghcid, where we
-- do not want it altering the environment for future runs.
devKeep :: HasKeep t => Ema.CLI.Action -> Mod t a
devKeep Ema.CLI.Run          = keep
devKeep (Ema.CLI.Generate _) = mempty
