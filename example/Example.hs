{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Main where

import           Data.Aeson
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.HTML.Blaze       (HTML)
import           Text.Blaze               (ToMarkup)

import           Servant.Seo.Combinators
import           Servant.Seo.Sitemap
import           Servant.Seo.UI

newtype NewsPage = NewsPage Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromHttpApiData, ToMarkup)

newtype NewsUrl = NewsUrl Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, ToHttpApiData, FromHttpApiData, ToMarkup)

newtype SearchResultPage = SearchResultPage Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromHttpApiData, ToMarkup)

newtype AboutPage = AboutPage Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromHttpApiData, ToMarkup)

newtype HomePage = HomePage Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromHttpApiData, ToMarkup)

newtype SearchPattern = SearchPattern Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, ToHttpApiData, FromHttpApiData)

newtype SearchResultsPage = SearchResultsPage Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, ToHttpApiData, FromHttpApiData, ToMarkup)

newtype BlogUrl = BlogUrl Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, ToHttpApiData, FromHttpApiData, ToMarkup)

newtype BlogPage = BlogPage Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromHttpApiData, ToMarkup)

newtype BlogComment = BlogComment Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromHttpApiData, FromJSON)

data Login = Login
    { username :: !Text
    , password :: !Text
    }
    deriving (Eq, Show, Generic, FromJSON)

newtype AdminPage = AdminPage Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromHttpApiData, ToMarkup)

-- *** User instances

instance ToSitemapPathPiece BlogUrl where
  getPathPiecesForIndexing _ _ = pure $ toUrl <$> ([0 .. 100000] :: [Int])
    where
      toUrl = BlogUrl . Text.pack . show

instance ToSitemapPathPiece NewsUrl where
  getPathPiecesForIndexing _ _ = pure $ toUrl <$> ([0 .. 100] :: [Int])
    where
      toUrl = NewsUrl . Text.pack . show

instance ToSitemapParamPart SearchPattern where
  getParamsForIndexing _ _ = pure $ SearchPattern . Text.pack <$> samples
    where
      samples = [ [ c1, c2 ] |  c1 <- ['a' .. 'z'], c2 <- ['0' .. '9'] ]

-- ** Example API

type PublicAPI
  =    Get '[HTML] HomePage
  :<|> ("blog" :> Frequency 'Always :> BlogAPI)
  :<|> ("news" :> Capture ":newsurl" NewsUrl :> Get '[HTML] NewsPage)
  :<|> ("search" :> QueryParam "q" SearchPattern :> Get '[HTML] SearchResultsPage)
  :<|> ("about" :> Priority '(1,0) :> Get '[HTML] AboutPage)
  :<|> "auth" :> ReqBody '[JSON] Login :> Post '[JSON] NoContent

type BlogAPI
  =    Capture ":blogurl" BlogUrl :> Get '[HTML] BlogPage
  :<|> Capture ":blogurl" BlogUrl
    :> ReqBody '[JSON] BlogComment
    :> Post '[JSON, HTML] BlogPage

type ProtectedAPI = Disallow "admin" :> Get '[HTML] AdminPage

type StaticAPI = "cdn" :> Disallow "static" :> Raw

type API = StaticAPI :<|> ProtectedAPI :<|> PublicAPI

-- ** Example app

api :: Proxy API
api = Proxy

server :: Server API
server
     = serveStatic
  :<|> serveProtected
  :<|> servePublic

serveProtected :: Handler AdminPage
serveProtected = throwError err401

servePublic :: Server PublicAPI
servePublic
     = serveHome
  :<|> (serveBlog :<|> servePostBlogComment)
  :<|> serveNews
  :<|> serveSearch
  :<|> serveAbout
  :<|> serveAuth

serveHome :: Handler HomePage
serveHome = pure (HomePage "")

serveBlog :: BlogUrl -> Handler BlogPage
serveBlog _ = pure (BlogPage "")

servePostBlogComment :: BlogUrl -> BlogComment -> Handler BlogPage
servePostBlogComment _ _ = pure (BlogPage "")

serveSearch :: Maybe SearchPattern -> Handler SearchResultsPage
serveSearch _ = pure (SearchResultsPage "")

serveAbout :: Handler AboutPage
serveAbout = pure (AboutPage "")

serveAuth :: Login -> Handler NoContent
serveAuth _ = pure NoContent

serveNews :: NewsUrl -> Handler NewsPage
serveNews _ = pure (NewsPage "")

serveStatic :: Server StaticAPI
serveStatic = serveDirectoryWebApp "."

startServer :: IO ()
startServer = do
  Warp.runSettings Warp.defaultSettings
    $ serveWithSeo website api server
  where
    website = "https://example.com"

main :: IO ()
main = startServer
