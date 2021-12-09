-- | Configuration settings passed in through env vars.
module EmaWiki.Config
  ( Config(..)
  , configParser
  ) where

import Relude
import Env


data Config = Config
  { algoliaApplicationId :: Text
  , algoliaIndexName :: Text
  , algoliaSearchOnlyApiKey :: Text
  } deriving (Eq,Show)

configParser :: (AsEmpty e, AsUnset e) => Parser e Config
configParser = Config
  <$> var (str <=< nonempty)
          "ALGOLIA_APPLICATION_ID"
          (help "ID for DocSearch app in Algolia")
  <*> var (str <=< nonempty)
          "ALGOLIA_INDEX_NAME"
          (help "Index name in Algolia")
  <*> var (str <=< nonempty)
          "ALGOLIA_SEARCH_ONLY_API_KEY"
          (help "Search-only API key for Algolia app")
