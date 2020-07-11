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
{-# LANGUAGE TypeOperators              #-}

module Servant.Seo.Robots where

import           Control.Lens
import           Data.Coerce             (coerce)
import           Data.Kind               (Type)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           GHC.Generics            (Generic)
import           GHC.TypeLits            (KnownNat, KnownSymbol, Nat, symbolVal)
import           Servant
import           Text.Blaze              (ToMarkup)

import           Servant.Seo.Combinators

-- * Robots.txt

-- ** RobotsInfo

-- | Intermediate structure representing @robots.txt@ file.
-- All API parts marked as 'Disallow' would be aggregated into 'RobotsInfo' during compilation and translated to @robots.txt@ content.
data RobotsInfo = RobotsInfo
    { _robotsSitemapPath     :: Maybe () -- ^ Indicate whether sitemap should be present in robots file or not.
    , _robotsDisallowedPaths :: [DisallowedPathPiece] -- ^ Path pieces that should be present in robots file.
    }
    deriving (Show, Eq, Generic)

-- | Part of URL that should be present in robots file.
newtype DisallowedPathPiece = DisallowedPathPiece Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (Ord, ToMarkup)

makeLenses ''RobotsInfo

-- | Empty unit of 'RobotsInfo'.
instance Monoid RobotsInfo where
  mempty = RobotsInfo Nothing []

instance Semigroup RobotsInfo where
  RobotsInfo s1 d1 <> RobotsInfo s2 d2 = RobotsInfo (s1 <> s2) (d1 <> d2)

-- ** HasRobots

-- | Servant API extension.
-- It describes how to build 'RobotsInfo' from servant API.
-- Most of types add nothing to it.
class HasRobots a where
  toRobots :: Proxy a -> RobotsInfo

instance HasRobots Raw where
  toRobots _ = mempty

instance HasRobots EmptyAPI where
  toRobots _ = mempty

-- | Collect different path pieces.
instance (HasRobots a, HasRobots b) => HasRobots (a :<|> b) where
  toRobots _ = toRobots (Proxy :: Proxy a) <> toRobots (Proxy :: Proxy b)

instance HasRobots sub => HasRobots (WithNamedContext x c sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots sub => HasRobots (HttpVersion :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots sub => HasRobots (StreamBody' mods fr ct a :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots sub => HasRobots (ReqBody' mods cs a :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots sub => HasRobots (RemoteHost :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots sub => HasRobots (QueryParam' mods sym a :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots sub => HasRobots (QueryParams sym a :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots sub => HasRobots (QueryFlag sym :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots sub => HasRobots (Header' mods sym a :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots sub => HasRobots (IsSecure :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots api => HasRobots (Summary desc :> api) where
  toRobots _ = toRobots (Proxy :: Proxy api)

instance HasRobots api => HasRobots (Description desc :> api) where
  toRobots _ = toRobots (Proxy :: Proxy api)

instance (KnownSymbol sym, HasRobots sub) =>
  HasRobots (Capture' mods sym a :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots sub => HasRobots (CaptureAll sym a :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

instance HasRobots sub => HasRobots (Vault :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub)

-- | Append path piece to existing 'DisallowedPathPiece'.
instance (HasRobots sub, KnownSymbol sym) => HasRobots (sym :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub) & decide piece
    where
      piece = (Text.append "/" . Text.pack . symbolVal) (Proxy :: Proxy sym)
      decide x ys@RobotsInfo{..} = case _robotsDisallowedPaths of
        [] | piece /= "/sitemap.xml" -> ys
           | otherwise -> ys & robotsSitemapPath ?~ ()
        _  -> ys & robotsDisallowedPaths .~ (DisallowedPathPiece .  Text.append x . coerce <$> (ys ^. robotsDisallowedPaths))

-- | Generate new 'DisallowedPathPiece' from path piece marked as 'Disallow'.
instance (HasRobots sub, KnownSymbol sym) => HasRobots (Disallow sym :> sub) where
  toRobots _ = toRobots (Proxy :: Proxy sub) & addPath piece
    where
      piece = (DisallowedPathPiece . Text.append "/" . Text.pack . symbolVal) (Proxy :: Proxy sym)
      addPath x xs = RobotsInfo Nothing [x] <> xs

instance {-# OVERLAPPABLE #-} (KnownNat status) => HasRobots (Verb method status cs NoContent) where
  toRobots _ = mempty

instance {-# OVERLAPPABLE #-} (KnownNat status) =>
  HasRobots (Verb method status cs a) where
    toRobots _ = mempty

instance {-# OVERLAPPABLE #-} (KnownNat status) => HasRobots (Verb (method :: Type) (status :: Nat) (cs :: [Type]) (Headers hs NoContent)) where
  toRobots _ = mempty

instance {-# OVERLAPPABLE #-} ( KnownNat status) => HasRobots (Verb (method :: Type) (status :: Nat) (cs :: [Type]) (Headers hs a)) where
  toRobots _ = mempty

-- | 'Frequency' as part of @sitemap.xml@ spec has no impact on @robots.txt@.
instance (HasPeriod period, HasRobots api) => HasRobots (Frequency period :> api) where
  toRobots _ = toRobots (Proxy :: Proxy api)

-- | 'Priority' as part of @sitemap.xml@ spec has no impact on @robots.txt@.
instance (HasRobots api) =>
  HasRobots (Priority priority :> api) where
    toRobots _ = toRobots (Proxy :: Proxy api)
