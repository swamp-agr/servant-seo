{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.Seo.Combinators where

import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as Text
import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic)
import           GHC.TypeLits         (KnownSymbol, Nat, Symbol)
import qualified Network.HTTP.Media   as M
import           Servant

-- ** XML

-- | Content-Type representing @text\/xml@. Used for serving @\/sitemap.xml@.
data XML deriving Typeable

-- | @text/xml@
instance Accept XML where
  contentType _ = "text" M.// "xml"

-- | @BSL.fromStrict . Text.encodeUtf8@
instance MimeRender XML Text where
  mimeRender _ = BSL.fromStrict . Text.encodeUtf8

-- | @BSL.fromStrict@
instance MimeRender XML ByteString where
  mimeRender _ = BSL.fromStrict

-- | @id@
instance MimeRender XML BSL.ByteString where
  mimeRender _ = id

-- ** Disallow

-- | Mark path as disallowed for indexing.
--
-- Example:
--
-- >>> -- GET /admin/crm
-- >>> type API = Disallow "admin" :> "crm" :> Get '[HTML] CrmPage
--
-- Code above will be transformed into @Disallow /admin@.
--
-- Note: 'Disallow' impacts @sitemap.xml@ excluding underlying URLs from resulted sitemap.
data Disallow (sym :: Symbol)

-- | 'Disallow' does not change specification at all.
instance (KnownSymbol sym, HasServer api context) => HasServer (Disallow sym :> api) context where
  type ServerT (Disallow sym :> api) m = ServerT (sym :> api) m

  route (Proxy :: Proxy (Disallow sym :> api)) context server =
    route (Proxy @(sym :> api)) context server

  hoistServerWithContext _ pc nt api =
    hoistServerWithContext (Proxy @(sym :> api)) pc nt api

-- ** Frequency

-- | 'Frequency' optional parameter for @sitemap.xml@. Shows to bots how often page will be changed.
-- Used with @Period@.
--
-- >>> type API = Frequency 'Yearly :> "about.php" :> Get '[HTML] AboutPage
--
-- Code above will be transformed in corresponding XML: @\<url\>\<loc\>https:\/\/example.com\/about.php\<\/loc\>\<freq\>yearly\<\/freq\>\<\/url\>@.
data Frequency (period :: Period)

-- | @Frequency@ does not change specification at all.
instance (HasPeriod period, HasServer api context) =>
  HasServer (Frequency period :> api) context where
    type ServerT (Frequency period :> api) m = ServerT api m

    route (Proxy :: Proxy (Frequency period :> api)) context server =
      route (Proxy @api) context server

    hoistServerWithContext _ pc nt api =
      hoistServerWithContext (Proxy @api) pc nt api

-- ** Period

-- | 'Period' is a type parameter for 'Frequency'.
data Period = Never
    | Yearly
    | Monthly
    | Weekly
    | Daily
    | Hourly
    | Always
    deriving (Show, Eq, Ord, Enum, Generic)

class HasPeriod a where
  getPeriod :: Proxy a -> Period

instance HasPeriod 'Never where getPeriod _ = Never

instance HasPeriod 'Yearly where getPeriod _ = Yearly

instance HasPeriod 'Monthly where getPeriod _ = Monthly

instance HasPeriod 'Weekly where getPeriod _ = Weekly

instance HasPeriod 'Daily where getPeriod _ = Daily

instance HasPeriod 'Hourly where getPeriod _ = Hourly

instance HasPeriod 'Always where getPeriod _  = Always


-- ** Priority

-- | 'Priority' optional parameter for @sitemap.xml@. Set priority on listed page to bots.
-- Possible values are between @'(0,0)@ and @'(1,0)@.
--
-- >>> type API = Priority '(1,0) :> "news.php" :> Get '[HTML] NewsPage
--
-- Code above will be transformed in corresponding XML: @\<url\>\<loc\>https:\/\/example.com\/news.php\<\/loc\>\<priority\>1.0\<\/priority\>\<\/url\>@.
data Priority (priority :: (Nat, Nat))

instance (HasServer api context) =>
  HasServer (Priority priority :> api) context where
    type ServerT (Priority priority :> api) m = ServerT api m

    route (Proxy :: Proxy (Priority priority :> api)) context server =
      route (Proxy @api) context server

    hoistServerWithContext _ pc nt api =
      hoistServerWithContext (Proxy @api) pc nt api

-- $setup
-- >>> :set -XDerivingStrategies -XGeneralizedNewtypeDeriving
-- >>> import Servant.HTML.Blaze (HTML)
-- >>> import Text.Blaze (ToMarkup)
-- >>> newtype CrmPage = CrmPage Text deriving newtype (ToMarkup)
-- >>> newtype AboutPage = AboutPage Text deriving newtype (ToMarkup)
-- >>> newtype NewsPage = NewsPage Text deriving newtype (ToMarkup)

