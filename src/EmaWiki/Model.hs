{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- | Internal representation of the wiki-alike.
module EmaWiki.Model
  ( MarkdownRoute(..)
  , indexMarkdownRoute
  , missingMarkdownRoute
  , mkMarkdownRoute
  , githubEditUrl
  , Model(..)
  , initModel
  , lookup
  , modelMember
  , modelInsert
  , modelDelete
  , Doc(..)
  , parseDoc
  , humanizeRoute
  , Meta(..)
  , doc404
  , mdUrl
  , forTags_
  , tagUrl
  ) where

import Relude
import qualified Data.Aeson as A
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Ema (Ema (..), Slug)
import qualified Ema
import qualified Ema.Helper.Markdown as Markdown
import EmaWiki.Config
import qualified EmaWiki.Pandoc
import System.FilePath (splitExtension, splitPath)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))


-- ------------------------
-- Our site route
-- ------------------------

-- | Represents the relative path to a source (.md) file under some directory.
--
-- We will reuse this in our site route type to refer to the corresponding .html.
--
-- If you are using this repo as a template, you might want to use an ADT as
-- route (eg: data Route = Index | About)
newtype MarkdownRoute = MarkdownRoute {unMarkdownRoute :: NonEmpty Slug}
  deriving (Eq, Ord, Show)

-- | Represents the top-level index.md
indexMarkdownRoute :: MarkdownRoute
indexMarkdownRoute = MarkdownRoute $ "index" :| []

-- | Customizable 404 page from 404.md
missingMarkdownRoute :: MarkdownRoute
missingMarkdownRoute = MarkdownRoute $ "404" :| []

-- | Convert foo/bar.md to a @MarkdownRoute@
--
-- If the file is not a Markdown file, return Nothing.
mkMarkdownRoute :: FilePath -> Maybe MarkdownRoute
mkMarkdownRoute = \case
  (splitExtension -> (fp, ".md")) ->
    let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath fp
     in MarkdownRoute <$> nonEmpty slugs
  _ ->
    Nothing

-- TODO: Use real URL utilities
githubEditUrl :: Config -> MarkdownRoute -> Text
githubEditUrl conf r = githubRepoUrl conf <>
  if r == missingMarkdownRoute
  then "/new/" <> githubEditBranch conf <> contentDir conf
  else "/edit/" <> githubEditBranch conf <> "/" <> contentDir conf <>
       if r == indexMarkdownRoute
       then "/index.md"
       else (T.intercalate "/" $ fmap Ema.unSlug $ toList $ unMarkdownRoute r) <> ".md"

-- ------------------------
-- Our site model
-- ------------------------

-- | This is our Ema "model" -- the app state used to generate our site.
--
-- It contains the list of all markdown files, parsed as Pandoc AST.
data Model = Model
  { modelDocs :: Map.Map MarkdownRoute Doc
  , modelTags :: TagMap
  }
  deriving (Eq, Show)

initModel :: Model
initModel = add404 $ Model mempty mempty
  where add404 = modelInsert missingMarkdownRoute doc404placeholder

data Doc = Doc
  { docTitle :: B.Inlines
  , docMeta :: Meta
  , docPandoc :: Pandoc
  } deriving (Eq, Show)

doc404placeholder :: Doc
doc404placeholder = Doc (B.str "404") def mempty


parseDoc :: MarkdownRoute -> FilePath -> Text -> Either Text Doc
parseDoc r fp s = uncurry (createDoc r) <$> parse fp s
  where parse = Markdown.parseMarkdownWithFrontMatter Markdown.fullMarkdownSpec

createDoc :: MarkdownRoute -> Maybe Meta -> Pandoc -> Doc
createDoc r mm pd = Doc t m pd''
  where
    (mt, pd') = EmaWiki.Pandoc.extractTitle pd
    t = fromMaybe (humanizeRoute r) mt
    m = fromMaybe def mm
    pd'' = EmaWiki.Pandoc.rewriteLinks urlTransform pd'

humanizeRoute :: MarkdownRoute -> B.Inlines
humanizeRoute = B.text . T.replace "_" " " . fold . fmap Ema.unSlug . unMarkdownRoute

urlTransform :: B.Attr -> [B.Inline] -> (Text, Text) -> (B.Attr, [B.Inline], (Text, Text))
urlTransform attr is ("", title) = (attr, is, (linkify is, title))
  where linkify = T.replace " " "_" . Markdown.plainify
urlTransform attr is ("/", title) = (attr, is, ("", title))
urlTransform attr is target = (attr, is, target)

data Meta = Meta
  -- | The list of tags extracted from the document’s front matter.
  { tags :: Tags
  }
  deriving (Eq, Show, Generic, A.FromJSON)

instance Default Meta where
  def = Meta mempty

lookup :: MarkdownRoute -> Model -> Maybe Doc
lookup r = Map.lookup r . modelDocs

modelMember :: MarkdownRoute -> Model -> Bool
modelMember k =
  Map.member k . modelDocs

modelInsert :: MarkdownRoute -> Doc -> Model -> Model
modelInsert k v model = Model modelDocs' modelTags'
  where
    modelDocs' = Map.insert k v (modelDocs model)
    modelTags' = insertTags k (tags $ docMeta v) $ modelTags model

type TagMap = Map Text (Set MarkdownRoute)

insertTags :: MarkdownRoute -> Tags -> TagMap -> TagMap
insertTags r (Tags ts) mp = foldl' (flip $ Map.alter f) mp ts
  where f :: Maybe (Set MarkdownRoute) -> Maybe (Set MarkdownRoute)
        f Nothing = Just $ Set.singleton r
        f (Just rs) = Just $ Set.insert r rs

modelDelete :: MarkdownRoute -> Model -> Model
modelDelete k model = Model modelDocs' modelTags'
  where
    modelDocs' = Map.delete k $ modelDocs model
    modelTags' = case Map.lookup k $ modelDocs model of
      Nothing -> modelTags model
      Just doc -> deleteTags k (tags $ docMeta doc) $ modelTags model

deleteTags :: MarkdownRoute -> Tags -> TagMap -> TagMap
deleteTags r (Tags ts) = flip (foldr $ Map.update f) ts
  where f :: Set MarkdownRoute -> Maybe (Set MarkdownRoute)
        f rs = let rs' = Set.delete r rs
               in if null rs' then Nothing else Just rs'

-- | Once we have a "model" and "route" (as defined above), we should define the
-- @Ema@ typeclass to tell Ema how to decode/encode our routes, as well as the
-- list of routes to generate the static site with.
--
-- We use `Either` to represent either a static file route or a Markdown
-- generated route.
instance Ema Model (Either FilePath MarkdownRoute) where
  encodeRoute _model = \case
    Left fp -> fp
    Right (MarkdownRoute slugs) ->
      toString $ T.intercalate "/" (Ema.unSlug <$> toList slugs) <> ".html"

  decodeRoute _model fp = do
    if "static/" `T.isPrefixOf` toText fp
      then pure $ Left fp
      else do
        if null fp
          then pure $ Right indexMarkdownRoute
          else do
            basePath <- T.stripSuffix ".html" (toText fp)
            slugs <- nonEmpty $ fromString . toString <$> T.splitOn "/" basePath
            pure $ Right $ MarkdownRoute slugs

  -- Routes to write when generating the static site.
  allRoutes (Map.keys . modelDocs -> mdRoutes) =
    [Left "static"]
      <> fmap Right mdRoutes


doc404 :: Model -> Doc
doc404 = fromMaybe doc404placeholder . lookup missingMarkdownRoute

mdUrl :: Ema model (Either FilePath r) => model -> r -> Text
mdUrl model r =
  Ema.routeUrl model $ Right @FilePath r


newtype Tags = Tags [Text]
  deriving (Eq, Show)

instance A.FromJSON Tags where
  parseJSON = A.withText "Tags" $
    pure . Tags . fmap T.strip . T.splitOn ","

instance Monoid Tags where
  mempty = Tags mempty

instance Semigroup Tags where
  (Tags t1) <> (Tags t2) = Tags $ t1 <> t2

forTags_ :: Monad m => Tags -> m () -> (Text -> m ()) -> m ()
forTags_ (Tags []) _ _ = pure ()
forTags_ (Tags (t:ts)) i f = f t >> forM_ ts (\e -> i >> f e)

tagUrl :: Text -> Text
tagUrl = ("/tags/" <>)
