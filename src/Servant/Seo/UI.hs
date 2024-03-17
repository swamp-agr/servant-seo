{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.Seo.UI where

import           Control.Lens            hiding (Context)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Coerce             (coerce)
import qualified Data.List               as List
import qualified Data.Map.Strict         as Map
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Servant
import           Servant.Seo.Combinators
import           Servant.Seo.Robots
import           Servant.Seo.Sitemap

-- * robots.txt

-- | Robots API.
-- Provides @\/robots.txt@.
type RobotsAPI = "robots.txt" :> Get '[PlainText] Text

-- | Extends API with 'RobotsAPI'.
apiWithRobots
  :: forall (api :: *) (context :: [*]). (HasServer api context, HasRobots api)
  => Proxy api
  -> Context context
  -> Proxy ( RobotsAPI :<|> api )
apiWithRobots _ _ = Proxy

-- | Provides "wrapper" around API.
-- Both API and corresponding 'Server' wrapped with 'RobotsAPI' and 'serveRobots' handler.
serveWithRobots
  :: forall (api :: *) (context :: [*]). (HasServer api context, HasRobots api, HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters)
  => ServerUrl
  -> Proxy api
  -> Context context
  -> Server api
  -> Application
serveWithRobots serverUrl proxy context appServer = serveWithContext extendedProxy context extendedServer
  where
    extendedProxy :: Proxy (RobotsAPI :<|> api)
    extendedProxy = apiWithRobots proxy context

    extendedServer :: Server (RobotsAPI :<|> api)
    extendedServer = serveRobots serverUrl (toRobots proxy) :<|> appServer

-- | Handler for 'RobotsAPI'.
serveRobots :: ServerUrl -> RobotsInfo -> Handler Text
serveRobots serverUrl robots = robots
  ^. robotsDisallowedPaths
  . to (fmap (Text.append "Disallow " . coerce))
  . to addUserAgent
  . to (addSitemap serverUrl)
  . to Text.unlines
  . to pure
  where
    addSitemap (ServerUrl url) r = if robots ^. robotsSitemapPath . to (== Nothing)
      then r
      else r <> ["", "Sitemap: " <> url <> "/sitemap.xml"]

    addUserAgent r = ["User-agent: *"] <> r

-- * sitemap.xml

-- | Sitemap API.
-- Provides both single @\/sitemap.xml@ and @\/sitemap\/:sitemap\/sitemap.xml@ in case of indexing.
-- If sitemap consists of more than 50000 URLs @\/sitemap.xml@ would return list of indeces to nested sitemaps.
type SitemapAPI
   =   "sitemap.xml" :> Get '[XML] BSL.ByteString
  :<|> "sitemap" :> Capture ":sitemap" SitemapIx :> "sitemap.xml" :> Get '[XML] BSL.ByteString

-- | Extends API with 'SitemapAPI'.
apiWithSitemap
  :: forall (api :: *) (context :: [*]). (HasServer api context, HasSitemap api)
  => Proxy api
  -> Context context
  -> Proxy ( SitemapAPI :<|> api )
apiWithSitemap _ _ = Proxy

-- | Provides "wrapper" around API.
-- Both API and corresponding 'Server' wrapped with 'SitemapAPI' and 'serveSitemap' handler.
serveWithSitemap
  :: forall (api :: *) (context :: [*]). (HasServer api context, HasSitemap api, HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters)
  => ServerUrl
  -> Proxy api
  -> Context context
  -> Server api
  -> Application
serveWithSitemap serverUrl proxy context appServer = serveWithContext extendedProxy context extendedServer
  where
    extendedProxy :: Proxy (SitemapAPI :<|> api)
    extendedProxy = apiWithSitemap proxy context

    extendedServer :: Server (SitemapAPI :<|> api)
    extendedServer = sitemapServer serverUrl proxy context :<|> appServer

-- | 'Server' implementation for @sitemap.xml@ and indexed sitemaps (if present).
sitemapServer
  :: forall (api :: *) (context :: [*]). (HasServer api context, HasSitemap api)
  => ServerUrl
  -> Proxy api
  -> Context context
  -> Server SitemapAPI
sitemapServer serverUrl proxy context = serveSitemap serverUrl proxy context
  :<|> serveNestedSitemap serverUrl proxy context

-- | Provides implementation for @sitemap.xml@.
serveSitemap
  :: forall (api :: *) (context :: [*]). (HasServer api context, HasSitemap api)
  => ServerUrl
  -> Proxy api
  -> Context context
  -> Handler BSL.ByteString
serveSitemap serverUrl proxy _ = do
  sitemap <- toSitemapInfo proxy
  pure $ sitemapUrlsToRootLBS serverUrl (urls sitemap)
  where
    urls x = x ^. sitemapInfoEntries . to (fmap (sitemapEntryToUrlList serverUrl))

-- | Provides implementation for nested sitemaps.
serveNestedSitemap
  :: forall (api :: *) (context :: [*]). (HasServer api context, HasSitemap api)
  => ServerUrl
  -> Proxy api
  -> Context context
  -> SitemapIx
  -> Handler BSL.ByteString
serveNestedSitemap serverUrl proxy _ (SitemapIx sitemapIndex) = do
  sitemap <- toSitemapInfo proxy
  let urls = getUrls sitemap
  if urls & concatMap _sitemapUrlLoc & length & (<= 50000)
  then throwError err404
  else case Map.lookup sitemapIndex (urlgroups urls) of
         Nothing      -> throwError err404
         Just content -> pure content
  where
    getUrls x = x ^. sitemapInfoEntries
      . to (fmap (sitemapEntryToUrlList serverUrl))
      . to List.sort
    urlgroups xs = sitemapUrlsToSitemapMap serverUrl xs


-- ** Both robots.txt and sitemap.xml

-- | Useful wrapper to extend API with both @robots.txt@ and @sitemap.xml@ with servant context.
serveWithSeo
  :: forall (api :: *) (context :: [*]). (HasServer api context, HasRobots api, HasSitemap api, HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters)
  => ServerUrl
  -> Proxy api
  -> Context context
  -> Server api
  -> Application
serveWithSeo serverUrl appProxy appContext appServer = serveWithContext extendedProxy appContext extendedServer
  where
    extendedProxy :: Proxy (RobotsAPI :<|> SitemapAPI :<|> api)
    extendedProxy = Proxy

    extendedServer :: Server (RobotsAPI :<|> SitemapAPI :<|> api)
    extendedServer = serveRobots serverUrl (toRobots (Proxy :: Proxy (SitemapAPI :<|> api)))
      :<|> sitemapServer serverUrl appProxy appContext
      :<|> appServer

-- | Useful wrapper to extend API with both @robots.txt@ and @sitemap.xml@ without servant context.
serveWithSeo'
  :: forall (api :: *). (HasServer api '[], HasRobots api, HasSitemap api)
  => ServerUrl
  -> Proxy api
  -> Server api
  -> Application
serveWithSeo' serverUrl appProxy appServer = serveWithSeo serverUrl appProxy EmptyContext appServer



