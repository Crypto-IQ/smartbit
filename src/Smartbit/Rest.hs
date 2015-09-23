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
           :> QueryParam "limit" Int
           :> QueryParam "dir" SortDir
           :> QueryParam "prev" Text
           :> QueryParam "next" Text          
           :> Get '[JSON] AddressesData
           
           :<|>

           "v1"
           :> "blockchain"
           :> "addresses"
           :> QueryParam "limit" Int
           :> QueryParam "sort" SortBy 
           :> QueryParam "dir" SortDir
           :> QueryParam "prev" Text
           :> QueryParam "next" Text
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

address ::       Addresses -> 
                 Maybe Int -> 
                 Maybe SortDir -> 
                 Maybe Text -> 
                 Maybe Text -> 
                 SmartbitT AddressesData
addresses ::     Maybe Int -> 
                 Maybe SortBy -> 
                 Maybe SortDir -> 
                 Maybe Text -> 
                 Maybe Text -> 
                 SmartbitT AddressesData
exchangerates :: SmartbitT ExchangeRates
pool ::          Text -> 
                 SmartbitT Pools
pools ::         SmartbitT Pools
totals ::        SmartbitT Totals

address 
  :<|> addresses
  :<|> exchangerates
  :<|> pool
  :<|> pools
  :<|> totals = client api (BaseUrl Https restHost restPort)

-----------------------------------------------------------------------------

address' :: Addresses -> SmartbitT AddressesData
address' as = address as Nothing Nothing Nothing Nothing

addresses' :: SmartbitT AddressesData
addresses' = addresses Nothing Nothing Nothing Nothing Nothing


