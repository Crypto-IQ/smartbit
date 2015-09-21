{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}

module Smartbit.Rest where

import           Control.Monad.Trans.Either
import           Data.Proxy
import           Data.Text
import           Servant.API
import           Servant.Client
import           Smartbit.Types

-----------------------------------------------------------------------------

restHost :: Host
restHost = "api.smartbit.com.au"

restPort :: Port
restPort = 443

-----------------------------------------------------------------------------

type SmartbitT = EitherT ServantError IO

-----------------------------------------------------------------------------

runSmartbit :: SmartbitT a -> IO (Either ServantError a)
runSmartbit = runEitherT

-----------------------------------------------------------------------------

type API = "v1" 
           :> "blockchain"
           :> "address"
           :> Capture "address" Addresses
           :> Get '[JSON] AddressesData
           
           :<|>
           
           "v1"
           :> "exchange-rates"
           :> Get '[JSON] ExchangeRates

           :<|>

           "v1"
           :> "blockchain"
           :> "pool"
           :> Capture "pool" Text
           :> Get '[JSON] Pools          
           
           :<|>

           "v1"
           :> "blockchain"
           :> "pools"
           :> Get '[JSON] Pools
           
           :<|>
           
           "v1"
           :> "blockchain"
           :> "totals"
           :> Get '[JSON] Totals

-----------------------------------------------------------------------------

api :: Proxy API
api = Proxy

-----------------------------------------------------------------------------

address :: Addresses -> SmartbitT AddressesData
xrates :: SmartbitT ExchangeRates
pool :: Text -> SmartbitT Pools
pools :: SmartbitT Pools
totals :: SmartbitT Totals

address 
  :<|> xrates
  :<|> pool
  :<|> pools
  :<|> totals = client api (BaseUrl Https restHost restPort)



