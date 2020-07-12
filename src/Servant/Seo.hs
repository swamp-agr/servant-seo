-- |
-- Module:      Servant.Seo
-- License:     BSD3
-- Maintainer:  Andrey Prokopenko <persiantiger@yandex.ru>
-- Stability:   experimental
--
-- This library provides useful SEO extension to servant ecosystem if you are intended to serve dynamic HTML pages with servant.
-- Extension consists of two parts:
--
-- 1. Auto-generating @robots.txt@ from API.
--
-- 2. Auto-generating @sitemap.xml@ from API.
--
-- This library provides only 'Disallow' and 'Sitemap' instructions from <https://www.robotstxt.org/robotstxt.html Robots specification> for all robots.
--
-- <https://www.sitemaps.org/protocol.html Sitemap.xml specification> was implemented almost completely except gzip archivation for sitemap indeces and custom namespaces.
module Servant.Seo
  ( -- * How to use this library
    -- $howto0

    -- ** Robots example #1
    -- $howto1

    -- ** Sitemap example #1
    -- $howto2

    -- ** Sitemap example #2
    -- $howto3

    -- ** Sitemap example #3
    -- $howto4

    -- ** Sitemap example #4
    -- $howto5

    -- ** Robots example #2
    -- $howto6

    -- ** Re-exports
    module Servant.Seo.Combinators
  , module Servant.Seo.Robots
  , module Servant.Seo.Sitemap
  , module Servant.Seo.UI
  ) where

import           Servant.Seo.Combinators
import           Servant.Seo.Robots
import           Servant.Seo.Sitemap
import           Servant.Seo.UI

-- $setup
-- >>> import Text.Blaze (ToMarkup)
-- >>> import Data.Text (Text)
-- >>> import Servant.API
-- >>> import Servant (Proxy (..))
-- >>> import Servant.Server (runHandler)
-- >>> import Servant.HTML.Blaze (HTML)
-- >>> import Data.Aeson (FromJSON)
-- >>> import GHC.Generics (Generic)
-- >>> import qualified Data.Text as Text
-- >>> import qualified Data.Text.IO as Text
-- >>> import qualified Data.ByteString.Lazy.Char8 as BSL8
-- >>> :set -XDerivingStrategies -XGeneralizedNewtypeDeriving -XDataKinds -XTypeOperators -XOverloadedStrings -XDeriveGeneric -XDeriveAnyClass
-- >>> newtype HomePage = HomePage Text deriving newtype (ToMarkup, FromHttpApiData)
-- >>> newtype AdminPage = AdminPage Text deriving newtype (ToMarkup, FromHttpApiData)
-- >>> data Login = Login { username :: Text, password :: Text } deriving (Eq, Show, Generic, FromJSON)
-- >>> newtype AboutPage = AboutPage Text deriving newtype (ToMarkup, FromHttpApiData)
-- >>> newtype NewsPage = NewsPage Text deriving newtype (ToMarkup, FromHttpApiData)
-- >>> newtype NewsUrl = NewsUrl Text deriving newtype (ToMarkup, ToHttpApiData, FromHttpApiData)
-- >>> type NewsAPI = "news" :> Capture ":newsurl" NewsUrl :> Get '[HTML] NewsPage
-- >>> type StaticAPI = "cdn" :> Disallow "static" :> Raw
-- >>> type ProtectedAPI = Disallow "admin" :> Get '[HTML] AdminPage
-- >>> let serverUrl = ServerUrl "https://example.com"
-- >>> type HomeAPI = Priority '(1,0) :> Frequency 'Monthly :> Get '[HTML] HomePage
-- >>> type AboutAPI = Frequency 'Yearly :> Priority '(0,1) :> "about" :> Get '[HTML] AboutPage
-- >>> type AuthAPI = "auth" :> ReqBody '[JSON] Login :> Post '[JSON] NoContent
-- >>> type PublicAPI = Get '[HTML] HomePage
-- >>> type API = PublicAPI :<|> StaticAPI :<|> ProtectedAPI

-- $howto0
--
-- This module describes how to extend servant API to get handlers for @/sitemap.xml@ and @/robots.txt@.

-- $howto1
--
-- Consider initial API:
--
-- >>> newtype HomePage = HomePage Text deriving newtype (ToMarkup, FromHttpApiData)
-- >>> newtype AdminPage = AdminPage Text deriving newtype (ToMarkup, FromHttpApiData)
-- >>> type PublicAPI = Get '[HTML] HomePage
-- >>> type StaticAPI = "cdn" :> "static" :> Raw
-- >>> type ProtectedAPI = "admin" :> Get '[HTML] AdminPage
-- >>> type API = PublicAPI :<|> StaticAPI :<|> ProtectedAPI
--
-- We want here to restrict robots on 'StaticAPI' and 'ProtectedAPI'.
--
-- >>> type StaticAPI = "cdn" :> Disallow "static" :> Raw
-- >>> type ProtectedAPI = Disallow "admin" :> Get '[HTML] AdminPage
-- >>> type API = PublicAPI :<|> StaticAPI :<|> ProtectedAPI
--
-- 'toRobots' function provides API analysis and produces 'RobotsInfo'.
--
-- >>> toRobots (Proxy :: Proxy API)
-- RobotsInfo {_robotsSitemapPath = Nothing, _robotsDisallowedPaths = [DisallowedPathPiece "/cdn/static",DisallowedPathPiece "/admin"]}
--
-- For 'RobotsInfo' you can use 'serveRobots' handler. @/robots.txt@ will be look like:
--
-- >>> let serverUrl = ServerUrl "https://example.com"
-- >>> Right robotsResponse <- runHandler (serveRobots (ServerUrl "https://example.com") (toRobots (Proxy :: Proxy API)))
-- >>> robotsResponse
-- "User-agent: *\nDisallow /cdn/static\nDisallow /admin\n"
--
--
-- Moreover, API could be easily extended with 'apiWithRobots'.
--
-- >>> :t apiWithRobots (Proxy :: Proxy API)
-- apiWithRobots (Proxy :: Proxy API) :: Proxy (RobotsAPI :<|> API)
--
-- 'serveWithRobots' provides extension for both initial API and its implementation with 'RobotsAPI'.

-- $howto2
--
-- Extend API from previous section with @POST \/auth@ route and @GET \/about@ page:
--
-- >>> data Login = Login { username :: Text, password :: Text } deriving (Eq, Show, Generic, FromJSON)
-- >>> newtype AboutPage = AboutPage Text deriving newtype (ToMarkup, FromHttpApiData)
-- >>> type PublicAPI = Get '[HTML] HomePage :<|> "about" :> Get '[HTML] AboutPage :<|> "auth" :> ReqBody '[JSON] Login :> Post '[JSON] NoContent
-- >>> type API = PublicAPI :<|> StaticAPI :<|> ProtectedAPI
--
-- Consider e.g., that Home page could be updated monthly and About page could be updated once per year.
-- Home page will have the highest priority. And about page will have the lowest one.
--
-- >>> type HomeAPI = Priority '(1,0) :> Frequency 'Monthly :> Get '[HTML] HomePage
-- >>> type AboutAPI = Frequency 'Yearly :> Priority '(0,1) :> "about" :> Get '[HTML] AboutPage
-- >>> type AuthAPI = "auth" :> ReqBody '[JSON] Login :> Post '[JSON] NoContent
-- >>> type PublicAPI = HomeAPI :<|> AboutAPI :<|> AuthAPI
-- >>> type API = PublicAPI :<|> StaticAPI :<|> ProtectedAPI
-- >>> toSitemapInfo (Proxy :: Proxy API)
-- SitemapInfo {_sitemapInfoEntries = [SitemapEntry {_sitemapPathPieces = [], _sitemapQueryParts = [], _sitemapFrequency = Just Monthly, _sitemapPriority = Just "1.0"},SitemapEntry {_sitemapPathPieces = [UrlPathPiece "about"], _sitemapQueryParts = [], _sitemapFrequency = Just Yearly, _sitemapPriority = Just "0.1"}], _sitemapInfoPresent = Just ()}
--
-- Use 'toSitemapInfo' to get the intermediate sitemap representation of API.
-- 'toSitemapInfo' will automatically skip all HTTP non-GET requests or other content types like @JSON@, @XML@, @PlainText@ and etc.
--
-- Only @Get '[HTML] a@ will be accepted.
--
-- For 'SitemapInfo' there is also 'serveSitemap' function.
--
-- >>> Right sitemapResponse <- runHandler $ serveSitemap serverUrl (Proxy :: Proxy API)
-- >>> BSL8.putStrLn sitemapResponse
-- <?xml version="1.0" encoding="UTF-8"?><urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"><url><loc>https://example.com</loc><changefreq>monthly</changefreq><priority>1.0</priority></url><url><loc>https://example.com/about</loc><changefreq>yearly</changefreq><priority>0.1</priority></url></urlset>
--
-- Again, there is helper 'apiWithSitemap'.
--
-- >>> :t apiWithSitemap (Proxy :: Proxy API)
-- apiWithSitemap (Proxy :: Proxy API) :: Proxy (SitemapAPI :<|> API)
--
-- 'serveWithSitemap' function will extend both API and corresponding handlers with 'SitemapAPI'.

-- $howto3
--
-- Consider more complex example dependent on user supplied types and their ranges of possible values.
--
-- >>> newtype NewsPage = NewsPage Text deriving newtype (ToMarkup, FromHttpApiData)
-- >>> newtype NewsUrl = NewsUrl Text deriving newtype (ToMarkup, ToHttpApiData, FromHttpApiData)
-- >>> type NewsAPI = "news" :> Capture ":newsurl" NewsUrl :> Get '[HTML] NewsPage
-- >>> type PublicAPI = HomeAPI :<|> AboutAPI :<|> AuthAPI :<|> NewsAPI
--
-- In order to obtain sitemap for user supplied types in API (like 'Capture'), you have to provide instance to 'ToSitemapPathPiece' type class for corresponding captured type, i.e. the way to get the list of available values that could lead to real pages. Otherwise, empty list will be supplied and such API branch would be ignored.
--
-- >>> instance ToSitemapPathPiece NewsUrl where getPathPiecesForIndexing _ _ = pure $ (NewsUrl . Text.pack . show) <$> [0 .. 10]
-- >>> toSitemapInfo (Proxy :: Proxy PublicAPI)
-- SitemapInfo {_sitemapInfoEntries = [SitemapEntry {_sitemapPathPieces = [], _sitemapQueryParts = [], _sitemapFrequency = Just Monthly, _sitemapPriority = Just "1.0"},SitemapEntry {_sitemapPathPieces = [UrlPathPiece "about"], _sitemapQueryParts = [], _sitemapFrequency = Just Yearly, _sitemapPriority = Just "0.1"},SitemapEntry {_sitemapPathPieces = [UrlPathPiece "news",CaptureValues ["0","1","2","3","4","5","6","7","8","9","10"]], _sitemapQueryParts = [], _sitemapFrequency = Nothing, _sitemapPriority = Nothing}], _sitemapInfoPresent = Just ()}
-- >>> Right sitemapResponse <- runHandler $ serveSitemap serverUrl (Proxy :: Proxy PublicAPI)
-- >>> BSL8.putStrLn sitemapResponse
-- <?xml version="1.0" encoding="UTF-8"?><urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"><url><loc>https://example.com</loc><changefreq>monthly</changefreq><priority>1.0</priority></url><url><loc>https://example.com/about</loc><changefreq>yearly</changefreq><priority>0.1</priority></url><url><loc>https://example.com/news/0</loc></url><url><loc>https://example.com/news/1</loc></url><url><loc>https://example.com/news/2</loc></url><url><loc>https://example.com/news/3</loc></url><url><loc>https://example.com/news/4</loc></url><url><loc>https://example.com/news/5</loc></url><url><loc>https://example.com/news/6</loc></url><url><loc>https://example.com/news/7</loc></url><url><loc>https://example.com/news/8</loc></url><url><loc>https://example.com/news/9</loc></url><url><loc>https://example.com/news/10</loc></url></urlset>
--
-- See 'ToSitemapPathPiece' for more details.
--

-- $howto4
--
-- Query parameters could be handled in the same way as captured path pieces of URL.
-- There is another type class for that: 'ToSitemapParamPart'.
--
-- >>> newtype SearchPattern = SearchPattern Text deriving newtype (ToMarkup, FromHttpApiData, ToHttpApiData)
-- >>> newtype SearchResultsPage = SearchResultsPage Text deriving newtype (ToMarkup)
-- >>> type SearchAPI = "search" :> QueryParam "q" SearchPattern :> Get '[HTML] SearchResultsPage
-- >>> instance ToSitemapParamPart SearchPattern where getParamsForIndexing _ _ = pure $ (SearchPattern . Text.pack) <$> [ [ c1, c2 ] |  c1 <- ['a' .. 'e'], c2 <- ['0' .. '3'] ]
-- >>> toSitemapInfo (Proxy :: Proxy SearchAPI)
-- SitemapInfo {_sitemapInfoEntries = [SitemapEntry {_sitemapPathPieces = [UrlPathPiece "search"], _sitemapQueryParts = [(ParamName "q",[ParamValue "a0",ParamValue "a1",ParamValue "a2",ParamValue "a3",ParamValue "b0",ParamValue "b1",ParamValue "b2",ParamValue "b3",ParamValue "c0",ParamValue "c1",ParamValue "c2",ParamValue "c3",ParamValue "d0",ParamValue "d1",ParamValue "d2",ParamValue "d3",ParamValue "e0",ParamValue "e1",ParamValue "e2",ParamValue "e3"])], _sitemapFrequency = Nothing, _sitemapPriority = Nothing}], _sitemapInfoPresent = Just ()}
-- >>> Right sitemapResponse <- runHandler $ serveSitemap serverUrl (Proxy :: Proxy SearchAPI)
-- >>> BSL8.putStrLn sitemapResponse
-- <?xml version="1.0" encoding="UTF-8"?><urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"><url><loc>https://example.com/search?q=a0</loc></url><url><loc>https://example.com/search?q=a1</loc></url><url><loc>https://example.com/search?q=a2</loc></url><url><loc>https://example.com/search?q=a3</loc></url><url><loc>https://example.com/search?q=b0</loc></url><url><loc>https://example.com/search?q=b1</loc></url><url><loc>https://example.com/search?q=b2</loc></url><url><loc>https://example.com/search?q=b3</loc></url><url><loc>https://example.com/search?q=c0</loc></url><url><loc>https://example.com/search?q=c1</loc></url><url><loc>https://example.com/search?q=c2</loc></url><url><loc>https://example.com/search?q=c3</loc></url><url><loc>https://example.com/search?q=d0</loc></url><url><loc>https://example.com/search?q=d1</loc></url><url><loc>https://example.com/search?q=d2</loc></url><url><loc>https://example.com/search?q=d3</loc></url><url><loc>https://example.com/search?q=e0</loc></url><url><loc>https://example.com/search?q=e1</loc></url><url><loc>https://example.com/search?q=e2</loc></url><url><loc>https://example.com/search?q=e3</loc></url></urlset>
--
-- See 'ToSitemapParamPart' for more details.
--

-- $howto5
--
-- Consider following API:
--
-- >>> instance ToSitemapPathPiece NewsUrl where getPathPiecesForIndexing _ _ = pure $ (NewsUrl . Text.pack . show) <$> [0 .. 50001]
-- >>> type PublicAPI = HomeAPI :<|> AboutAPI :<|> AuthAPI :<|> NewsAPI
-- >>> Right sitemapResponse <- runHandler $ serveSitemap serverUrl (Proxy :: Proxy PublicAPI)
-- >>> BSL8.putStrLn sitemapResponse
-- <?xml version="1.0" encoding="UTF-8"?><sitemapindex xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"><sitemap><loc>https://example.com/0/sitemap.xml</loc></sitemap><sitemap><loc>https://example.com/1/sitemap.xml</loc></sitemap><sitemap><loc>https://example.com/2/sitemap.xml</loc></sitemap><sitemap><loc>https://example.com/3/sitemap.xml</loc></sitemap></sitemapindex>
--
-- If sitemap page has more than 50000 URLs, it should be replaced with sitemap index page according to sitemap specification.
-- Each URL inside XML would be accesible. Look on 'SitemapAPI' definition for more details.

-- $howto6
--
-- >>> type API = PublicAPI :<|> StaticAPI :<|> ProtectedAPI
-- >>> Right robotsResponse <- runHandler (serveRobots (ServerUrl "https://example.com") (toRobots $ apiWithSitemap (Proxy :: Proxy API)))
-- >>> robotsResponse
-- "User-agent: *\nDisallow /cdn/static\nDisallow /admin\n\nSitemap: https://example.com/sitemap.xml\n"
--
-- API extended with sitemap will automatically be populated with link to sitemap xml page.
-- To serve both robots and sitemap in advance to your API look for 'serveWithSeo' helper function.
