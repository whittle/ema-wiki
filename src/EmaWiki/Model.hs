{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Internal representation of the wiki-alike.
module EmaWiki.Model
  ( MarkdownRoute(..)
  , indexMarkdownRoute
  , missingMarkdownRoute
  , mkMarkdownRoute
  , markdownRouteSourcePath
  , markdownRouteFileBase
  , markdownRouteInits
  , Model(..)
  , modelLookup
  , modelLookupMeta
  , modelMember
  , modelInsert
  , modelDelete
  , Meta(..)
  ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Tree (Tree)
import Ema (Ema (..), Slug)
import qualified Ema
import qualified Ema.Helper.PathTree as PathTree
import System.FilePath (splitExtension, splitPath)
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

markdownRouteSourcePath :: MarkdownRoute -> FilePath
markdownRouteSourcePath r =
  if r == indexMarkdownRoute
    then "index.md"
    else toString (T.intercalate "/" $ fmap Ema.unSlug $ toList $ unMarkdownRoute r) <> ".md"

-- | Filename of the markdown file without extension
markdownRouteFileBase :: MarkdownRoute -> Text
markdownRouteFileBase =
  Ema.unSlug . head . NE.reverse . unMarkdownRoute

-- | For use in breadcrumbs
markdownRouteInits :: MarkdownRoute -> NonEmpty MarkdownRoute
markdownRouteInits (MarkdownRoute ("index" :| [])) =
  one indexMarkdownRoute
markdownRouteInits (MarkdownRoute (slug :| rest')) =
  indexMarkdownRoute :| case nonEmpty rest' of
    Nothing ->
      one $ MarkdownRoute (one slug)
    Just rest ->
      MarkdownRoute (one slug) : go (one slug) rest
  where
    go :: NonEmpty Slug -> NonEmpty Slug -> [MarkdownRoute]
    go x (y :| ys') =
      let this = MarkdownRoute (x <> one y)
       in case nonEmpty ys' of
            Nothing ->
              one this
            Just ys ->
              this : go (unMarkdownRoute this) ys

-- ------------------------
-- Our site model
-- ------------------------

-- | This is our Ema "model" -- the app state used to generate our site.
--
-- It contains the list of all markdown files, parsed as Pandoc AST.
data Model = Model
  { modelDocs :: Map MarkdownRoute (Meta, Pandoc),
    modelNav :: [Tree Slug]
  }
  deriving (Eq, Show)

instance Default Model where
  def = Model mempty mempty

data Meta = Meta
  { -- | Indicates the order of the Markdown file in sidebar tree, relative to
    -- its siblings.
    order :: Maybe Int
  }
  deriving (Eq, Show, Generic, FromJSON)

instance Default Meta where
  def = Meta Nothing

modelLookup :: MarkdownRoute -> Model -> Maybe Pandoc
modelLookup k =
  fmap snd . Map.lookup k . modelDocs

modelLookupMeta :: MarkdownRoute -> Model -> Meta
modelLookupMeta k =
  maybe def fst . Map.lookup k . modelDocs

modelMember :: MarkdownRoute -> Model -> Bool
modelMember k =
  Map.member k . modelDocs

modelInsert :: MarkdownRoute -> (Meta, Pandoc) -> Model -> Model
modelInsert k v model =
  let modelDocs' = Map.insert k v (modelDocs model)
   in model
        { modelDocs = modelDocs',
          modelNav =
            PathTree.treeInsertPathMaintainingOrder
              (\k' -> order $ maybe def fst $ Map.lookup (MarkdownRoute k') modelDocs')
              (unMarkdownRoute k)
              (modelNav model)
        }

modelDelete :: MarkdownRoute -> Model -> Model
modelDelete k model =
  model
    { modelDocs = Map.delete k (modelDocs model),
      modelNav = PathTree.treeDeletePath (unMarkdownRoute k) (modelNav model)
    }

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
