{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smartbit.Types where

-----------------------------------------------------------------------------

import           Control.Applicative              ((<|>))
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Base58String.Bitcoin as B58
import qualified Data.Bitcoin.Types        as B
import           Data.Foldable                    (asum)
import           Data.Scientific
import           Data.Text                        (Text)
import           Data.Text                 as T   (intercalate)
import           Servant.Common.Text              (ToText(..))

-----------------------------------------------------------------------------

type Host = String
type Port = Int

-----------------------------------------------------------------------------

newtype Addresses = Addresses { addresses :: [B.Address]}

instance ToText Addresses where
  toText =  T.intercalate "," . map B58.toText . addresses

-----------------------------------------------------------------------------

data AddressStats = AddressStats
  { _astReceived :: Scientific
  , _astSpent :: Scientific
  , _astBalance :: Scientific
  } deriving Show

instance FromJSON AddressStats where
  parseJSON = withObject "address stats" $ \o -> do
    _astReceived <- read <$> o .: "received"
    _astSpent <- read <$> o .: "spent"
    _astBalance <- read <$> o .: "balance"
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
  , _xrRate :: Scientific
  } deriving Show

parseExchangeRate :: Value -> Parser ExchangeRate
parseExchangeRate = withObject "exchange rate" $ \o -> do
  _xrCode <- o .: "code"
  _xrName <- o .: "name"
  _xrRate <- o .: "rate"
  return ExchangeRate{..}

instance FromJSON ExchangeRate where
  parseJSON = parseExchangeRate

-----------------------------------------------------------------------------

newtype ExchangeRates = ExchangeRates
  {_xrRates :: [ExchangeRate]
  } deriving Show

parseExchangeRates :: Value -> Parser ExchangeRates
parseExchangeRates = withObject "exchange rates" $ \o -> do
  x <- o .: "exchange_rates"
  xrs <- parseJSON x
  return $ ExchangeRates xrs

instance FromJSON ExchangeRates where
  parseJSON = parseExchangeRates

-----------------------------------------------------------------------------

data Totals = Totals
  { _totBlockCount :: Scientific
  , _totTxnCount :: Scientific
  , _totCurrency :: Scientific
  }
  deriving Show

parseTotals :: Value -> Parser Totals
parseTotals = withObject "totals" $ \o -> do
  x <- o .: "totals"
  _totBlockCount <- x .: "block_count"
  _totTxnCount <- x .: "transaction_count"
  _totCurrency <- read <$> x .: "currency"
  return Totals{..}

instance FromJSON Totals where
  parseJSON = parseTotals

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
