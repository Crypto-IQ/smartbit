{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smartbit.Types where

-----------------------------------------------------------------------------

import           Control.Applicative              ((<|>))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import qualified Data.Base58String.Bitcoin as B58
import qualified Data.Bitcoin.Types        as B
import           Data.Foldable                    (asum)
import           Data.Scientific
import           Data.Text                        (Text)
import           Data.Text                 as T   (intercalate,pack,unpack)
import           GHC.Generics
import           Servant.Common.Text              (ToText(..))

-----------------------------------------------------------------------------

type Host = String
type Port = Int

-----------------------------------------------------------------------------

newtype BTC = BTC
  { unBTC :: Scientific
  }
  deriving
  ( Eq, Fractional, Ord, Num, Real, RealFrac, Read, Show)

parseBTC :: Value -> Parser BTC
parseBTC = withText "BTC" $ \v ->
  case reads (T.unpack v) of
    ((b,_):_) -> return (BTC b)
    _         -> fail "Could not parse text to scientific"

instance FromJSON BTC where
  parseJSON = parseBTC

-----------------------------------------------------------------------------

newtype Addresses = Addresses
  { _addresses :: [B.Address]
  } deriving Show

instance ToText Addresses where
  toText =  T.intercalate "," . map B58.toText . _addresses

-----------------------------------------------------------------------------

data AddressStats = AddressStats
  { _astReceived :: BTC
  , _astSpent :: BTC
  , _astBalance :: BTC
  } deriving Show

instance FromJSON AddressStats where
  parseJSON = withObject "address stats" $ \o -> do
    _astReceived <- o .: "received"
    _astSpent <- o .: "spent"
    _astBalance <- o .: "balance"
    return AddressStats{..}

-----------------------------------------------------------------------------

data AddressData = AddressData
  { _addAddress :: B.Address
  , _addTotal :: AddressStats
  } deriving Show

parseAddressData :: Value -> Parser AddressData
parseAddressData = withObject "address data" $ \o -> do
  _addAddress <- o .: "address"
  t <- o .: "total"
  _addTotal <- parseJSON t  
  return AddressData{..}

instance FromJSON AddressData where
  parseJSON = parseAddressData

-----------------------------------------------------------------------------

data AddressesData = AddressesData
  { _adsData :: [AddressData]
  } deriving Show

parseSingleAddress :: Value -> Parser AddressesData
parseSingleAddress = withObject "single address data" $ \o -> do
  ad <- o .: "address"
  return $ AddressesData [ad]

parseManyAddresses :: Value -> Parser AddressesData
parseManyAddresses = withObject "many addresses data" $ \o -> do
  _adsData <- o .: "addresses"
  return AddressesData{..}

instance FromJSON AddressesData where
  parseJSON v = parseSingleAddress v <|> 
                parseManyAddresses v

-----------------------------------------------------------------------------

data ExchangeRate = ExchangeRate
  { _xrCode :: Text
  , _xrName :: Text
  , _xrRate :: BTC
  } deriving (Generic,Show)

instance FromJSON ExchangeRate where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-----------------------------------------------------------------------------

newtype ExchangeRates = ExchangeRates
  { _xrRates :: [ExchangeRate]
  } deriving Show

parseExchangeRates :: Value -> Parser ExchangeRates
parseExchangeRates = withObject "exchange rates" $ \o -> do
  x <- o .: "exchange_rates"
  _xrRates <- parseJSON x
  return $ ExchangeRates{..}

instance FromJSON ExchangeRates where
  parseJSON = parseExchangeRates

-----------------------------------------------------------------------------

data Pool = Pool
  { _plName :: Text
  , _plLink :: Text
  , _plBlockCount :: Int
  } deriving (Generic,Show)

instance FromJSON Pool where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

-----------------------------------------------------------------------------

newtype Pools = Pools
  { _pools :: [Pool]
  } deriving Show

parseSinglePool :: Value -> Parser Pools
parseSinglePool = withObject "pool" $ \o -> do
  ps <- o .: "pool"
  p <- parseJSON ps
  return $ Pools [p]

parseManyPools :: Value -> Parser Pools
parseManyPools = withObject "pools" $ \o -> do
  ps <- o .: "pools"
  _pools <- parseJSON ps
  return Pools{..}

instance FromJSON Pools where
  parseJSON v = parseSinglePool v <|>
                parseManyPools v

-----------------------------------------------------------------------------

data SortBy = 
    Balance
  | Address
  | Received
  | Spent
  | TransactionCcount
  | InputCount
  | OutputCount
    deriving (Show)

instance ToText SortBy where
  toText = T.pack . snakeCase . show

-----------------------------------------------------------------------------

data SortDir =
    Asc
  | Desc
    deriving Show

instance ToText SortDir where
  toText = T.pack . snakeCase . show  

-----------------------------------------------------------------------------

data SubscriptionRequest = 
    SubscribeNewTxns
  | UnsubscribeNewTxns
  | SubscribeNewBlocks
  | UnsubscribeNewBlocks
    deriving Show

instance ToJSON SubscriptionRequest where
  toJSON r = case r of
    SubscribeNewTxns -> object ["type" .= ("new-transaction" :: Text)]
    UnsubscribeNewTxns -> object ["type" .= ("new-transaction" :: Text), "unsubscribe" .= True]
    SubscribeNewBlocks -> object ["type" .= ("new-block" :: Text)]
    UnsubscribeNewBlocks -> object ["type" .= ("new-block" :: Text), "unsubscribe" .= True]

-----------------------------------------------------------------------------

data SubscriptionResponse =
    NewTransaction
    { _txnId :: Text
    }
  | NewBlock
    { _blkHash :: Text
    }
  | Subscription
    { _subSuccess :: Bool
    , _subMessage :: Text
    }
    deriving Show

parseNewBlock :: Value -> Parser SubscriptionResponse
parseNewBlock = withObject "new block" $ \o -> do
  ("new-block" :: Text) <- o .: "type"
  p <- o .: "payload"
  _blkHash <- p .: "hash"
  return NewBlock{..}

parseNewTransaction :: Value -> Parser SubscriptionResponse
parseNewTransaction = withObject "new transaction" $ \o -> do
  ("new-transaction" :: Text) <- o .: "type"
  p <- o .: "payload"
  _txnId <- p .: "txid"
  return NewTransaction{..}

parseSubscription :: Value -> Parser SubscriptionResponse
parseSubscription = withObject "subscription" $ \o -> do
  ("subscribe-response" :: Text) <- o .: "type"
  p <- o .: "payload"
  _subSuccess <- p .: "success"
  _subMessage <- p .: "message"
  return Subscription{..}

responseParsers :: [Value -> Parser SubscriptionResponse]
responseParsers = [ parseNewBlock
                  , parseNewTransaction
                  , parseSubscription
                  ]

instance FromJSON SubscriptionResponse where
  parseJSON v = asum $ responseParsers <*> [v]

-----------------------------------------------------------------------------

data Totals = Totals
  { _totBlockCount :: Int
  , _totTxnCount :: Int
  , _totCurrency :: BTC
  }
  deriving Show

parseTotals :: Value -> Parser Totals
parseTotals = withObject "totals" $ \o -> do
  x <- o .: "totals"
  _totBlockCount <- x .: "block_count"
  _totTxnCount <- x .: "transaction_count"
  _totCurrency <- x .: "currency"
  return Totals{..}

instance FromJSON Totals where
  parseJSON = parseTotals
