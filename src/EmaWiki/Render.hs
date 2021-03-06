{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | This code generates a site based on Markdown files, rendering them using Pandoc.
module EmaWiki.Render
  ( render
  ) where

import Relude
import Control.Exception (throw)
import qualified Data.Text as T
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.Markdown as Markdown
import qualified Ema.Helper.Tailwind as Tailwind
import EmaWiki.Model (Model(..), MarkdownRoute(..))
import qualified EmaWiki.Config as Config
import qualified EmaWiki.Model as Model
import qualified EmaWiki.Pandoc
import NeatInterpolation (text)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))


render :: Config.Config -> Ema.CLI.Action -> Model -> Either FilePath MarkdownRoute -> Ema.Asset LByteString
render conf act model = \case
  Left fp ->
    -- This instructs ema to treat this route "as is" (ie. a static file; no generation)
    -- The argument `fp` refers to the absolute path to the static file.
    Ema.AssetStatic fp
  Right r ->
    -- Generate a Html route; hot-reload is enabled.
    Ema.AssetGenerated Ema.Html $ renderHtml conf act model r

renderHtml :: Config.Config -> Ema.CLI.Action -> Model -> MarkdownRoute -> LByteString
renderHtml conf emaAction model route =
  let doc = fromMaybe (Model.doc404 model) $ Model.lookup r model
      r = if Model.modelMember route model then route else Model.missingMarkdownRoute
   in Tailwind.layoutWith "en" "UTF-8"
                          (tailwindShim emaAction)
                          (headHtml conf emaAction r doc)
                          (bodyHtml conf model r doc)

-- | In ema v0.2.0.0, Ema.Helper.Tailwind.twindShimCdn points to
-- tailwindcss@latest, which as of v3.0.0 doesn’t include a dist dir.
tailwindShim :: Ema.CLI.Action -> H.Html
tailwindShim (Ema.CLI.Generate _) = Tailwind.twindShimUnofficial
tailwindShim Ema.CLI.Run = H.link
  ! A.href "https://unpkg.com/tailwindcss@2.2.19/dist/tailwind.css"
  ! A.rel "stylesheet"
  ! A.type_ "text/css"

headHtml :: Config.Config -> Ema.CLI.Action -> MarkdownRoute -> Model.Doc -> H.Html
headHtml conf emaAction r doc = do
  case emaAction of
    Ema.CLI.Generate _ ->
      -- Since our URLs are all relative, and GitHub Pages uses a non-root base
      -- URL, we should specify it explicitly. Note that this is not necessary if
      -- you are using a CNAME.
      H.base ! A.href (H.toValue $ Config.wikiRootUrl conf)
    _ ->
      H.base ! A.href "/"
  H.title $ H.text $ EmaWiki.Pandoc.plainify (Model.docTitle doc) <> " - Ema"
  favIcon
  -- Make this a PWA and w/ https://web.dev/themed-omnibox/
  H.link ! A.rel "manifest" ! A.href "manifest.json"
  H.meta ! A.name "theme-color" ! A.content "#DB2777"
  unless (r == Model.indexMarkdownRoute) prismJs
  algoliaJs
  where
    prismJs = do
      H.unsafeByteString . encodeUtf8 $
        [text|
        <link href="https://cdn.jsdelivr.net/npm/prismjs@1.23.0/themes/prism-tomorrow.css" rel="stylesheet" />
        <script src="https://cdn.jsdelivr.net/combine/npm/prismjs@1.23.0/prism.min.js,npm/prismjs@1.23.0/plugins/autoloader/prism-autoloader.min.js"></script>
        |]
    favIcon = do
      H.unsafeByteString . encodeUtf8 $
        [text|
        <link href="static/logo.svg" rel="icon" />
        |]
    algoliaJs =
      let appId = Config.algoliaApplicationId conf
          indexName = Config.algoliaIndexName conf
          apiKey = Config.algoliaSearchOnlyApiKey conf
      in H.unsafeByteString $ encodeUtf8
      [text|
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@docsearch/css@alpha" />
      <script src="https://cdn.jsdelivr.net/npm/@docsearch/js@alpha"></script>
      <script type="module">
        import * as dsmodule from 'https://cdn.jsdelivr.net/npm/@docsearch/js@alpha';
        docsearch({
          container: '.search-box',
          appId: '${appId}',
          indexName: '${indexName}',
          apiKey: '${apiKey}',
        });
      </script>
      <link rel="preconnect" href="https://JSX8B7PVKY-dsn.algolia.net" crossorigin />
      |]

data ContainerType
  = -- | The row representing title part of the site
    CHeader
  | -- | The row representing the main part of the site. Sidebar lives here, as well as <main>
    CBody
  deriving (Eq, Show)

containerLayout :: ContainerType -> H.Html -> H.Html -> H.Html
containerLayout ctype sidebar w = do
  H.div ! A.class_ "px-2 grid grid-cols-12" $ do
    let sidebarCls = case ctype of
          CHeader -> ""
          CBody -> "md:sticky md:top-0 md:h-screen overflow-x-auto"
    H.div ! A.class_ ("hidden md:mr-4 md:block md:col-span-3 " <> sidebarCls) $ do
      sidebar
    H.div ! A.class_ "col-span-12 md:col-span-9" $ do
      w

bodyHtml :: Config.Config -> Model -> MarkdownRoute -> Model.Doc -> H.Html
bodyHtml conf model r doc = do
  H.div ! A.class_ "container mx-auto xl:max-w-screen-lg" $ do
    -- Header row
    let sidebarLogo =
          H.div ! A.class_ "mt-2 h-full flex pl-2 space-x-2 items-end" $ do
            H.a ! A.href (H.toValue $ Model.mdUrl model Model.indexMarkdownRoute) $
              H.img ! A.class_ "z-50 transition transform hover:scale-125 hover:opacity-80 h-20" ! A.src "static/logo.svg"
    containerLayout CHeader sidebarLogo $ do
      H.div ! A.class_ "flex justify-center items-center" $ do
        H.h1 ! A.class_ "text-6xl mt-2 mb-2 text-center pb-2" $ mapM_ rpInline $ Model.docTitle doc
    -- Main row
    containerLayout CBody (H.div ! A.class_ "bg-yellow-50 rounded pt-1 pb-2" $ renderSidebarNav model r) $ do
      renderPandoc $ Model.docPandoc doc
      renderTagList $ Model.docMeta doc
      H.footer ! A.class_ "flex justify-center items-center space-x-4 my-8 text-center text-gray-500" $ do
        let editUrl = H.toValue $ Model.githubEditUrl conf r
        H.a ! A.href editUrl ! A.title "Edit this page on GitHub" $ editIcon
        H.div $ do
          "Powered by "
          H.a ! A.class_ "font-bold" ! A.href "https://github.com/srid/ema" $ "Ema"
  where
    editIcon =
      H.unsafeByteString $
        encodeUtf8
          [text|
          <svg xmlns="http://www.w3.org/2000/svg" class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor">
            <path d="M17.414 2.586a2 2 0 00-2.828 0L7 10.172V13h2.828l7.586-7.586a2 2 0 000-2.828z" />
            <path fill-rule="evenodd" d="M2 6a2 2 0 012-2h4a1 1 0 010 2H4v10h10v-4a1 1 0 112 0v4a2 2 0 01-2 2H4a2 2 0 01-2-2V6z" clip-rule="evenodd" />
          </svg>
          |]

renderSidebarNav :: Model -> MarkdownRoute -> H.Html
renderSidebarNav _model _currentRoute = do
  H.div ! A.class_ "pl-2 search-box" $ mempty
  H.div ! A.class_ "pl-2" $ H.text "Tools"

renderTagList :: Model.Meta -> H.Html
renderTagList (Model.tags -> ts) = when (ts /= mempty) $ do
  H.hr
  H.div $ do
    H.text "tags: "
    Model.forTags_ ts (H.text ", ") $ \t ->
      H.a ! A.class_ "text-yellow-600 hover:underline"
          ! A.href (H.toValue $ Model.tagUrl t) $ H.text t

-- ------------------------
-- Pandoc renderer
-- ------------------------
--
-- Note that we hardcode tailwind classes, because pandoc AST is not flexible
-- enough to provide attrs for all inlines/blocks. So we can't rely on Walk to
-- transform it.

renderPandoc :: Pandoc -> H.Html
renderPandoc (Pandoc _meta blocks) =
  mapM_ rpBlock blocks

rpBlock :: B.Block -> H.Html
rpBlock = \case
  B.Plain is ->
    mapM_ rpInline is
  B.Para is ->
    H.p ! A.class_ "my-2" $ mapM_ rpInline is
  B.LineBlock iss ->
    forM_ iss $ \is ->
      mapM_ rpInline is >> "\n"
  B.CodeBlock (id', classes, attrs) s ->
    -- Prism friendly classes
    let classes' = flip concatMap classes $ \cls -> [cls, "language-" <> cls]
     in H.div ! A.class_ "py-0.5 text-sm" $ H.pre ! rpAttr (id', classes', attrs) $ H.code ! rpAttr ("", classes', []) $ H.text s
  B.RawBlock (B.Format fmt) html ->
    if fmt == "html"
      then H.unsafeByteString $ encodeUtf8 html
      else throw Unsupported
  B.BlockQuote bs ->
    H.blockquote $ mapM_ rpBlock bs
  B.OrderedList _ bss ->
    H.ol ! A.class_ (listStyle <> " list-decimal") $
      forM_ bss $ \bs ->
        H.li ! A.class_ listItemStyle $ mapM_ rpBlock bs
  B.BulletList bss ->
    H.ul ! A.class_ (listStyle <> " list-disc") $
      forM_ bss $ \bs ->
        H.li ! A.class_ listItemStyle $ mapM_ rpBlock bs
  B.DefinitionList defs ->
    H.dl $
      forM_ defs $ \(term, descList) -> do
        mapM_ rpInline term
        forM_ descList $ \desc ->
          H.dd $ mapM_ rpBlock desc
  B.Header level attr is ->
    headerElem level ! rpAttr attr $ mapM_ rpInline is
  B.HorizontalRule ->
    H.hr
  B.Table {} ->
    throw Unsupported
  B.Div attr bs ->
    H.div ! rpAttr attr $ mapM_ rpBlock bs
  B.Null ->
    pure ()
  where
    listStyle = "list-inside ml-2 space-y-1 "
    listItemStyle = ""

headerElem :: Int -> H.Html -> H.Html
headerElem = \case
  1 -> H.h1 ! A.class_ "text-6xl mt-2 mb-2 text-center pb-2"
  2 -> H.h2 ! A.class_ ("text-5xl " <> my)
  3 -> H.h3 ! A.class_ ("text-4xl " <> my)
  4 -> H.h4 ! A.class_ ("text-3xl " <> my)
  5 -> H.h5 ! A.class_ ("text-2xl " <> my)
  6 -> H.h6 ! A.class_ ("text-xl " <> my)
  _ -> error "Invalid pandoc header level"
  where
    my = "mt-4 mb-2 text-gray-700"

rpInline :: B.Inline -> H.Html
rpInline = \case
  B.Str s -> H.toHtml s
  B.Emph is ->
    H.em $ mapM_ rpInline is
  B.Strong is ->
    H.strong $ mapM_ rpInline is
  B.Underline is ->
    H.u $ mapM_ rpInline is
  B.Strikeout is ->
    H.del $ mapM_ rpInline is
  B.Superscript is ->
    H.sup $ mapM_ rpInline is
  B.Subscript is ->
    H.sub $ mapM_ rpInline is
  B.Quoted qt is ->
    flip inQuotes qt $ mapM_ rpInline is
  B.Code attr s ->
    H.code ! rpAttr attr $ H.toHtml s
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> H.br
  B.RawInline _fmt s ->
    H.pre $ H.toHtml s
  B.Math _ _ ->
    throw Unsupported
  B.Link attr is (url, title) -> do
    let (cls, target) =
          if "://" `T.isInfixOf` url
            then ("text-yellow-600 hover:underline", targetBlank)
            else ("text-yellow-600 font-bold hover:bg-pink-50", mempty)
    H.a
      ! A.class_ cls
      ! A.href (H.textValue url)
      ! A.title (H.textValue title)
      ! target
      ! rpAttr attr
      $ mapM_ rpInline is
  B.Image attr is (url, title) ->
    H.img ! A.src (H.textValue url) ! A.title (H.textValue title) ! A.alt (H.textValue $ Markdown.plainify is) ! rpAttr attr
  B.Note _ ->
    throw Unsupported
  B.Span attr is ->
    H.span ! rpAttr attr $ mapM_ rpInline is
  x ->
    H.pre $ H.toHtml $ show @Text x
  where
    inQuotes :: H.Html -> B.QuoteType -> H.Html
    inQuotes w = \case
      B.SingleQuote -> "‘" >> w <* "’"
      B.DoubleQuote -> "“" >> w <* "”"

targetBlank :: H.Attribute
targetBlank =
  A.target "_blank" <> A.rel "noopener"

rpAttr :: B.Attr -> H.Attribute
rpAttr (id', classes, attrs) =
  let cls = T.intercalate " " classes
   in unlessNull id' (A.id (fromString . toString $ id'))
        <> unlessNull cls (A.class_ (fromString . toString $ cls))
        <> mconcat (fmap (\(k, v) -> H.dataAttribute (fromString . toString $ k) (fromString . toString $ v)) attrs)
  where
    unlessNull x f =
      if T.null x then mempty else f

data Unsupported = Unsupported
  deriving (Show, Exception)
