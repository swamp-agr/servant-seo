{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Servant.Seo.Sitemap where

import           Control.Lens
import           Control.Monad.IO.Class  (MonadIO)
import qualified Data.Binary.Builder     as Builder
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Coerce             (coerce)
import qualified Data.Foldable           as F
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (catMaybes)
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           GHC.Generics            (Generic)
import           GHC.TypeLits            (KnownNat, KnownSymbol, natVal,
                                          symbolVal)
import           Numeric                 (showFFloat)
import           Servant
import           Servant.HTML.Blaze      (HTML)
import           Text.Blaze              (ToMarkup)
import           Text.XML

import           Servant.Seo.Combinators

-- * Sitemap

-- ** SitemapInfo

-- | Intermediate structure representing @sitemap.xml@ file.
-- During compilation API analysis performed.
-- All 'HTML' pages with 'Get' verb would be populated into 'SitemapInfo' and then updated to list of URLs, unless 'Disallow' happened in the path to page.
-- It consists of list of API branches and internal flag.
data SitemapInfo = SitemapInfo
    { _sitemapInfoEntries :: [SitemapEntry]
    , _sitemapInfoPresent :: Maybe ()
    }
    deriving (Show, Eq, Generic, Ord)

instance Monoid SitemapInfo where
  mempty = SitemapInfo [] Nothing

instance Semigroup SitemapInfo where
  SitemapInfo e1 p1 <> SitemapInfo e2 p2
    = SitemapInfo (e1 <> e2) (p1 <> p2)

-- | Represents single API branch.
-- Could contain multiple values based on user decision.
-- See 'ToSitemapParamPart' or 'ToSitemapPathPiece' for more details.
-- Also contain optional 'Frequency' and 'Priority'.
data SitemapEntry = SitemapEntry
    { _sitemapPathPieces :: [PathPiece]
    , _sitemapQueryParts :: [(ParamName, [ParamValue])]
    , _sitemapFrequency  :: Maybe Period
    , _sitemapPriority   :: Maybe Text
    }
    deriving (Show, Eq, Generic, Ord)

instance Monoid SitemapEntry where
  mempty = SitemapEntry [] [] Nothing Nothing

instance Semigroup SitemapEntry where
  SitemapEntry p1 q1 f1 r1 <> SitemapEntry p2 q2 f2 r2 =
    SitemapEntry (p1 <> p2) (q1 <> q2) (maximum [f1, f2]) (maximum [r1, r2])

newtype ParamName = ParamName Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (Ord, ToMarkup, ToHttpApiData)

newtype ParamValue = ParamValue Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (Ord, ToMarkup, ToHttpApiData)

-- | Could be either path piece obtained from @path :: Symbol@ or list of possible captured values provided by user.
data PathPiece = UrlPathPiece Text
    | CaptureValues [Text]
    deriving (Show, Eq, Generic, Ord)

-- | Checks that path piece is contained.
isEmpty :: PathPiece -> Bool
isEmpty (UrlPathPiece "")  = True
isEmpty (CaptureValues []) = True
isEmpty _                  = False

-- ** User dependent decisions for sitemap.

-- | How to get all possible values that should (or should not) be the parts of sitemap from query parameter values.
--
-- Example (in combination with @Get '[HTML]@ will produce sitemap URLs):
--
-- >>> newtype ArticleId = ArticleId Int deriving newtype (Show, Eq, Enum, Ord, Num, ToHttpApiData)
-- >>> instance ToSitemapParamPart ArticleId where getParamsForIndexing _ _ = pure [0..100500]
--
-- Another example (params that have no affect on sitemap):
--
-- >>> newtype SortBy = SortBy Text deriving newtype (Show, Eq, ToHttpApiData)
-- >>> instance ToSitemapParamPart SortBy where getParamsForIndexing _ _ = pure mempty
--
class ToHttpApiData a => ToSitemapParamPart a where
  -- ^ Should be provided by user.
  getParamsForIndexing :: MonadIO m => Proxy a -> app -> m [a]
  getParamsForIndexing _ _ = pure mempty

  -- ^ Default implementation of conversion to '[ParamValue]'.
  toParamPart :: MonadIO m => Proxy a -> env -> m [ParamValue]
  toParamPart proxy env = do
    values <- getParamsForIndexing proxy env
    pure $ ParamValue . toUrlPiece <$> values

-- | How to get all possible captured values that should (or should not) be the parts of sitemap.
--
-- Example (in combination with @Get '[HTML]@ will produce sitemap URLs):
--
-- >>> newtype UserId = UserId Int deriving newtype (Show, Eq, Enum, Ord, Num, ToHttpApiData)
-- >>> instance ToSitemapPathPiece UserId where getPathPiecesForIndexing _ _ = pure [1..200000]
--
-- Another example (captured values that have no affect on sitemap):
--
-- >>> newtype Username = Username Text deriving newtype (Show, Eq, ToHttpApiData)
-- >>> instance ToSitemapPathPiece Username where getPathPiecesForIndexing _ _ = pure mempty
--
class ToHttpApiData a => ToSitemapPathPiece a where
  -- ^ Should be provided by user.
  getPathPiecesForIndexing :: MonadIO m => Proxy a -> app -> m [a]
  getPathPiecesForIndexing _ _ = pure mempty

  -- ^ Default implementation of conversion to 'PathPiece'.
  toPathPiece :: MonadIO m => Proxy a -> env -> m PathPiece
  toPathPiece proxy env = do
    values <- getPathPiecesForIndexing proxy env
    pure $ CaptureValues (toUrlPiece <$> values)

makeLenses ''SitemapInfo
makeLenses ''SitemapEntry

-- ** Transforming API to @SitemapInfo@

-- | Servant API extension.
-- It describes how to build 'SitemapInfo' representation from servant API.
-- There are plenty of types that add nothing to it.
--
-- __WARNING__: Do not derive this using @DeriveAnyClass@ as the generated
-- instance will loop indefinitely.
class HasSitemap a where
  toSitemapInfo :: MonadIO m => Proxy a -> m SitemapInfo
  toSitemapInfo proxy = toSitemapInfoWith () proxy

  toSitemapInfoWith :: MonadIO m => env -> Proxy a -> m SitemapInfo
  toSitemapInfoWith _ = toSitemapInfo

  {-# MINIMAL toSitemapInfo | toSitemapInfoWith #-}

instance HasSitemap Raw where
  toSitemapInfo _ = pure mempty

instance HasSitemap EmptyAPI where
  toSitemapInfo _ = pure mempty

-- | Collect multiple API branches together.
instance (HasSitemap a, HasSitemap b) => HasSitemap (a :<|> b) where
  toSitemapInfo _ = do
    sitemapA <- toSitemapInfo (Proxy :: Proxy a)
    sitemapB <- toSitemapInfo (Proxy :: Proxy b)
    pure (sitemapA <> sitemapB)

instance HasSitemap sub => HasSitemap (WithNamedContext x c sub) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy sub)

instance HasSitemap sub => HasSitemap (HttpVersion :> sub) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy sub)

instance HasSitemap sub => HasSitemap (StreamBody' mods fr ct a :> sub) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy sub)

instance HasSitemap sub => HasSitemap (ReqBody' mods cs a :> sub) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy sub)

instance HasSitemap sub => HasSitemap (RemoteHost :> sub) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy sub)

-- | Extract all possible values under 'QueryParam'' and append them if sitemap is present for this particular branch.
instance (ToSitemapParamPart a, HasSitemap sub, KnownSymbol sym) =>
  HasSitemap (QueryParam' mods sym a :> sub) where
    toSitemapInfoWith env _ = do
      sitemap <- toSitemapInfo (Proxy :: Proxy sub)
      vals <- toParamPart (Proxy :: Proxy a) env
      let newSitemap
            | sitemap == mempty = mempty
            | null vals = sitemap
            | otherwise
            = sitemap & sitemapInfoEntries .~ newEntries sitemap vals
      pure newSitemap
      where
        param = (ParamName . Text.pack . symbolVal) (Proxy :: Proxy sym)

        addQueryParts paramName paramValues x = x & sitemapQueryParts .~ new
          where
            new = x ^. sitemapQueryParts . to ((paramName, paramValues) :)
        newEntries x vals = if x ^. sitemapInfoEntries . to null
          then [addQueryParts param vals mempty]
          else x ^. sitemapInfoEntries . to (fmap (addQueryParts param vals))

instance HasSitemap sub => HasSitemap (QueryFlag sym :> sub) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy sub)

instance HasSitemap sub => HasSitemap (Header' mods sym a :> sub) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy sub)

instance HasSitemap sub => HasSitemap (IsSecure :> sub) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy sub)

instance HasSitemap api => HasSitemap (Summary desc :> api) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy api)

instance HasSitemap api => HasSitemap (Description desc :> api) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy api)

-- | Extract all possible values under 'Capture'' and append them if sitemap is present for this particular branch.
instance (HasSitemap sub, ToSitemapPathPiece a) =>
  HasSitemap (Capture' mods sym a :> sub) where
    toSitemapInfoWith env _ = do
      sitemap <- toSitemapInfo (Proxy :: Proxy sub)
      value <- toPathPiece (Proxy @a) env
      pure $ if sitemap == mempty
        then mempty
        else
          if isEmpty value
            then sitemap
            else sitemap & sitemapInfoEntries .~ newEntries sitemap value

      where
        addPathPieces capturedValue x = x & sitemapPathPieces .~ new
          where
            new = x ^. sitemapPathPieces . to (capturedValue :)

        newEntries x capturedValue = if x ^. sitemapInfoEntries . to null
          then [addPathPieces capturedValue mempty]
          else x ^. sitemapInfoEntries . to (fmap (addPathPieces capturedValue))

instance HasSitemap sub => HasSitemap (CaptureAll sym a :> sub) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy sub)

instance HasSitemap sub => HasSitemap (Vault :> sub) where
  toSitemapInfo _ = toSitemapInfo (Proxy :: Proxy sub)

instance (HasSitemap sub, KnownSymbol sym) => HasSitemap (sym :> sub) where
  toSitemapInfo _ = do
    sitemap <- toSitemapInfo (Proxy :: Proxy sub)
    pure $ if sitemap == mempty
      then mempty
      else sitemap & sitemapInfoEntries .~ newEntries sitemap
    where
      newEntries x = if x ^. sitemapInfoEntries . to null
        then [addPathPiece piece mempty]
        else x ^. sitemapInfoEntries . to (fmap (addPathPiece piece))
      piece = (UrlPathPiece . Text.pack . symbolVal) (Proxy :: Proxy sym)
      addPathPiece p x = x & sitemapPathPieces .~ new
        where
          new = x ^. sitemapPathPieces . to (p :)

-- | 'Disallow' combinator invalidates sitemap for particular API branch.
instance (HasSitemap sub, KnownSymbol sym) => HasSitemap (Disallow sym :> sub) where
  toSitemapInfo _ = pure mempty

instance {-# OVERLAPPABLE #-} (KnownNat status) =>
  HasSitemap (Verb method status cs a) where
    toSitemapInfo _ = pure mempty

-- | @Get '[HTML]@ enables sitemap for particular API branch.
instance {-# OVERLAPPING #-} (ToMarkup a) => HasSitemap (Get '[HTML] a) where
  toSitemapInfo _ = pure (SitemapInfo [mempty] (Just ()))

-- | Extracts 'Frequency' from API branch.
instance (HasPeriod period, HasSitemap api) => HasSitemap (Frequency period :> api) where
  -- FIXME: compare with previous values, choose frequent one.
  toSitemapInfo _ = do
    sitemap <- toSitemapInfo (Proxy :: Proxy api)
    pure $ sitemap & sitemapInfoEntries . each %~ (sitemapFrequency . _Just .~ period')
    where
      period' = getPeriod (Proxy :: Proxy period)

-- | Extracts 'Priority' from API branch.
instance (KnownNat n, KnownNat m, HasSitemap api) => HasSitemap (Priority '(n, m) :> api) where
  toSitemapInfo _ = do
    sitemap <- toSitemapInfo (Proxy :: Proxy api)
    -- FIXME: compare with previous values, choose greater one.
    pure $ sitemap & sitemapInfoEntries . each %~ (sitemapPriority . _Just .~ priority')
    where
      n' = natVal (Proxy :: Proxy n) & fromInteger @Float
      m' = natVal (Proxy :: Proxy m) & fromInteger @Float
      priority' = (n' * 10 + m') / 10
        & min 1.0 & showFloat & Text.pack

      showFloat x = showFFloat (Just 1) x ""

-- ** Rendering

-- | Populated during 'SitemapInfo' processing.
-- 'SitemapUrl' would be rendered in XML node in runtime.
--
-- Example: @\<url\>\<loc\>https:\/\/example.com\/some\/url\<\/loc\>@.
data SitemapUrl = SitemapUrl
    { _sitemapUrlLoc       :: [SitemapLoc]
    , _sitemapUrlFrequency :: Maybe Period
    , _sitemapUrlPriority  :: Maybe Text
    }
    deriving (Show, Eq, Generic, Ord)

-- | Represents single URL listed in @sitemap.xml@.
newtype SitemapLoc = SitemapLoc Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (Ord, ToMarkup)

-- | Server prefix without trailing slash.
newtype ServerUrl = ServerUrl Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (Ord, ToMarkup, IsString)

-- | If sitemap consists of more than 50000 URLs, it should be indexed.
type SitemapIndex = (Int, [SitemapUrl])

newtype SitemapIx = SitemapIx Int
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, FromHttpApiData, ToHttpApiData)

instance ToSitemapPathPiece SitemapIx

makeLenses ''SitemapUrl

-- | Transform single sitemap entry to list of URLs.
sitemapEntryToUrlList :: ServerUrl -> SitemapEntry -> SitemapUrl
sitemapEntryToUrlList server SitemapEntry {..} = SitemapUrl
  { _sitemapUrlLoc       = locs
  , _sitemapUrlFrequency = _sitemapFrequency
  , _sitemapUrlPriority  = _sitemapPriority
  }
  where
    locs = case (null paths, null queries) of
      (False, False) -> [ SitemapLoc (coerce server <> path <> query) | path <- paths, query <- queries ]
      (True, False)  -> [ SitemapLoc (coerce server <> "/" <> query) | query <- queries ]
      (False, True)  -> [ SitemapLoc (coerce server <> path) | path <- paths ]
      _              -> [ SitemapLoc (coerce server) ]
    paths = _sitemapPathPieces
      & F.foldr combinePaths []
      & fmap (escapeXmlEntities . toText)
      where
        combinePaths :: PathPiece -> [Builder] -> [Builder]
        combinePaths p [] = case p of
          UrlPathPiece piece   -> [prepare piece]
          CaptureValues pieces -> prepare <$> pieces
        combinePaths p xs = case p of
          UrlPathPiece piece -> prepend piece <$> xs
          CaptureValues pieces -> concatMap (\piece -> prepend piece <$> xs) pieces

        prepare = Builder.append (Builder.fromByteString "/") . toEncodedUrlPiece

        prepend x = Builder.append (prepare x)

    queries = _sitemapQueryParts
      & F.foldl' combineParams []
      & fmap (escapeXmlEntities . fixQuery . toText)
      where
        combineParams :: [Builder] -> (ParamName, [ParamValue]) -> [Builder]
        combineParams [] (param, values) = values
          & fmap toEncodedUrlPiece
          & fmap (combine param)
        combineParams xs paramWithValues = combineParams [] paramWithValues
          & concatMap (\piece -> Builder.append piece <$> xs)

        combine p v = Builder.fromByteString "&"
          <> toEncodedUrlPiece p
          <> Builder.fromByteString "="
          <> v

        fixQuery q = if Text.null q then q else Text.cons '?' (Text.tail q)

    toText :: Builder -> Text
    toText = Text.decodeUtf8 . BSL.toStrict . Builder.toLazyByteString

    escapeXmlEntities txt = txt
      & Text.replace "&"  "&amp;"
      & Text.replace "'"  "&apos;"
      & Text.replace "\"" "&quot;"
      & Text.replace ">"  "&gt;"
      & Text.replace "<"  "&lt;"

-- | Transform list of URLs to list of XML nodes.
sitemapUrlToNodes :: SitemapUrl -> [Node]
sitemapUrlToNodes SitemapUrl{..} = mkUrlNode "url" extra <$> _sitemapUrlLoc
  where
    extra = [ frequencyNode, priorityNode ] & catMaybes
    frequencyNode = fromFrequency <$> _sitemapUrlFrequency
    priorityNode  = fromPriority  <$> _sitemapUrlPriority

    fromFrequency = mkNodeWithContent "changefreq" . Text.toLower . Text.pack . show
    fromPriority = mkNodeWithContent "priority" . coerce

-- | Several XML rendering helpers.
mkNode :: Name -> [Node] -> Node
mkNode name childNodes = NodeElement (Element name Map.empty childNodes)

mkNodeWithContent :: Name -> Text -> Node
mkNodeWithContent name content = mkNode name [NodeContent content]

mkUrlNode :: Name -> [Node] -> SitemapLoc -> Node
mkUrlNode name extra loc = mkNode name (locNode : extra)
  where
    locNode = mkNodeWithContent "loc" (coerce loc)

-- | Transform list of URLs to sitemap index XML nodes.
sitemapIndexUrlToNodes :: SitemapUrl -> [Node]
sitemapIndexUrlToNodes SitemapUrl{..} = mkUrlNode "sitemap" [] <$> _sitemapUrlLoc

-- | Transform bunch of list of URLs to XML document.
sitemapUrlsToDocument :: ServerUrl -> [SitemapUrl] -> Document
sitemapUrlsToDocument server urlparts = Document
  { documentPrologue = Prologue [] Nothing []
  , documentRoot = Element "urlset" (Map.fromList [namespace]) childNodes
  , documentEpilogue = []
  }
  where
    namespace = ("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")
    childNodes = concatMap sitemapUrlToNodes urlparts

-- | Transform list of sitemap index URLs to XML document.
sitemapIndexToDocument :: ServerUrl -> [SitemapUrl] -> Document
sitemapIndexToDocument server urlparts = Document
  { documentPrologue = Prologue [] Nothing []
  , documentRoot = Element "sitemapindex" (Map.fromList [namespace]) childNodes
  , documentEpilogue = []
  }
  where
    namespace = ("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")
    childNodes = concatMap sitemapIndexUrlToNodes urlparts

sitemapUrlsToRootLBS :: ServerUrl -> [SitemapUrl] -> BSL.ByteString
sitemapUrlsToRootLBS serverUrl urls = if urls & concatMap _sitemapUrlLoc & length & (<= 50000)
  then render sitemapUrlsToDocument urls
  else render sitemapIndexToDocument indeces
  where
    render x ys = renderSitemapWith x serverUrl ys
    indeces = urls & concatMap countLocGroups & length & pred & mkIndexList & fmap mkIndexUrl
    countLocGroups :: SitemapUrl -> [Int]
    countLocGroups x = x
      & _sitemapUrlLoc
      & length
      & (realToFrac @_ @Double)
      & (/ 50000)
      & truncate
      & mkIndexList
    mkIndexList x = [0 .. x]
    mkIndexUrl x = SitemapUrl
      { _sitemapUrlLoc = pure $ SitemapLoc
          $ coerce serverUrl <> "/" <> Text.pack (show x) <> "/sitemap.xml"
      , _sitemapUrlFrequency = Nothing
      , _sitemapUrlPriority = Nothing
      }

sitemapUrlsToSitemapMap :: ServerUrl -> [SitemapUrl] -> Map Int BSL.ByteString
sitemapUrlsToSitemapMap serverUrl urls = urls
  & concatMap (splitUrlTo50KLocs [])
  & zip [0..]
  & Map.fromList
  where
    splitUrlTo50KLocs xs s = if s ^. sitemapUrlLoc . to length . to (<= 50000)
      then mkLBS [s] : xs
      else splitUrlTo50KLocs (currentLocsSitemap : xs) restLocsSitemap
      where
        currentLocsSitemap = mkLBS [s & sitemapUrlLoc %~ take 50000]
        restLocsSitemap = s & sitemapUrlLoc %~ drop 50000
    mkLBS = renderSitemapWith sitemapUrlsToDocument serverUrl

renderSitemapWith
  :: (ServerUrl -> [SitemapUrl] -> Document) -> ServerUrl -> [SitemapUrl] -> BSL.ByteString
renderSitemapWith renderer serverUrl urls = renderer serverUrl urls & renderLBS def

-- $setup
-- >>> :set -XDerivingStrategies -XGeneralizedNewtypeDeriving
-- >>>
