{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}

module Smartbit.Rest where

import           Control.Monad.Trans.Either
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Text
import           Servant.API
import           Servant.Client
import           Smartbit.Servant
import           Smartbit.Types
import           Smartbit.Util

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

type APIVersion = "v1"
type Blockchain = "blockchain"

type SortBy'Address   = Select '[SortBy TxnIndex']

type SortBy'Addresses = Select '[ SortBy Address'
                                , SortBy Balance'
                                , SortBy InputCount'
                                , SortBy OutputCount'
                                , SortBy Received'
                                , SortBy Spent'
                                , SortBy TxnCount'
                                ]

type SortBy'Block     = Select '[ SortBy BlockIndex'
                                , SortBy Fee'
                                , SortBy InputAmount'
                                , SortBy InputCount'
                                , SortBy OutputAmount'
                                , SortBy OutputCount'
                                , SortBy Size'
                                , SortBy TxnId'
                                ]

-----------------------------------------------------------------------------

type API = APIVersion
           :> Blockchain
           :> "address"
           :> Capture "address" Addresses
           :> CaptureOpt "txnfilter" TxnFilter
           :> QueryParam "limit" Int
           :> QueryParam "sort" SortBy'Address 
           :> QueryParam "dir" SortDir
           :> QueryParam "prev" Text
           :> QueryParam "next" Text
           :> Get '[JSON] Value

           :<|>
           
           APIVersion
           :> Blockchain
           :> "addresses"
           :> QueryParam "limit" Int
           :> QueryParam "sort" SortBy'Addresses
           :> QueryParam "dir" SortDir
           :> QueryParam "prev" Text
           :> QueryParam "next" Text
           :> Get '[JSON] AddressesData
           
           :<|>

           APIVersion
           :> Blockchain
           :> "block"
           :> Capture "blockheight" Int
           :> QueryParam "limit" Int
           :> QueryParam "sort" SortBy'Block
           :> QueryParam "dir" SortDir
           :> QueryParam "prev" Text
           :> QueryParam "next" Text
           :> Get '[JSON] Value

           :<|>

           APIVersion
           :> "exchange-rates"
           :> Get '[JSON] ExchangeRates

           :<|>

           APIVersion
           :> Blockchain
           :> "pool"
           :> Capture "pool" Text
           :> Get '[JSON] Pools          
           
           :<|>

           APIVersion
           :> Blockchain
           :> "pools"
           :> Get '[JSON] Pools
           
           :<|>
           
           APIVersion
           :> Blockchain
           :> "totals"
           :> Get '[JSON] Totals


-----------------------------------------------------------------------------

api :: Proxy API
api = Proxy

-----------------------------------------------------------------------------

address ::       Addresses ->
                 Maybe TxnFilter ->
                 Maybe Int ->
                 Maybe SortBy'Address ->
                 Maybe SortDir ->
                 Maybe Text ->
                 Maybe Text ->
                 SmartbitT Value

addresses ::     Maybe Int ->
                 Maybe SortBy'Addresses ->
                 Maybe SortDir ->
                 Maybe Text ->
                 Maybe Text ->
                 SmartbitT AddressesData

block ::         Int -> 
                 Maybe Int ->
                 Maybe SortBy'Block ->
                 Maybe SortDir ->
                 Maybe Text ->
                 Maybe Text ->
                 SmartbitT Value

exchangerates :: SmartbitT ExchangeRates

pool ::          Text ->
                 SmartbitT Pools

pools ::         SmartbitT Pools

totals ::        SmartbitT Totals

address
  :<|> addresses
  :<|> block
  :<|> exchangerates
  :<|> pool
  :<|> pools
  :<|> totals = client api (BaseUrl Https restHost restPort)

-----------------------------------------------------------------------------

address' :: Addresses -> SmartbitT Value
address' as = address as Nothing Nothing Nothing Nothing Nothing Nothing

addresses' :: SmartbitT AddressesData
addresses' = addresses Nothing Nothing Nothing Nothing Nothing

block' :: Int -> SmartbitT Value
block' h = block h Nothing Nothing Nothing Nothing Nothing
