{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Smartbit.Servant where

import           Data.Proxy
import           Data.Text                  (unpack)
import           Data.Typeable              (Typeable)
import           GHC.TypeLits
import           Servant.API
import           Servant.Client
import           Servant.Common.Req

-----------------------------------------------------------------------------

data CaptureOpt (sym :: Symbol) a deriving (Typeable)

instance (KnownSymbol capture, ToText a, HasClient sublayout)
  => HasClient (CaptureOpt capture a :> sublayout) where
  type Client (CaptureOpt capture a :> sublayout) = 
    Maybe a -> Client sublayout
  clientWithRoute Proxy req baseurl val = clientWithRoute
    (Proxy :: Proxy sublayout)
    (maybe req (flip appendToPath req . unpack . toText) val)
    baseurl

