{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Smartbit.Socket where

import           Control.Monad.Reader
import           Data.Aeson
import           Network.WebSockets   (Connection,receiveData,sendTextData)
import           Smartbit.Types
import           Wuss

-----------------------------------------------------------------------------

wsHost :: Host
wsHost = "ws.smartbit.com.au"

wsPort :: Port
wsPort = 443

-----------------------------------------------------------------------------

newtype SubscriptionT a = SubscriptionT 
  { unSubscriptionT :: ReaderT Connection IO a}
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader Connection
  )

-----------------------------------------------------------------------------

runSubscription :: SubscriptionT a -> IO a
runSubscription =
  runSecureClient wsHost (fromIntegral wsPort) "/v1/blockchain" 
    . runReaderT 
      . unSubscriptionT

-----------------------------------------------------------------------------

send :: SubscriptionRequest -> SubscriptionT ()
send req = SubscriptionT $ do
  conn <- ask
  liftIO . sendTextData conn . encode $ req

recv :: SubscriptionT (Maybe SubscriptionResponse)
recv = SubscriptionT $ do 
  conn <- ask
  bs <- liftIO $ receiveData conn
  return (decode bs)

-----------------------------------------------------------------------------

subNewTxns :: SubscriptionT ()
subNewTxns = send SubscribeNewTxns

unsubNewTxns :: SubscriptionT ()
unsubNewTxns = send UnsubscribeNewTxns

subNewBlocks :: SubscriptionT ()
subNewBlocks = send SubscribeNewBlocks

unsubNewBlocks :: SubscriptionT ()
unsubNewBlocks = send UnsubscribeNewBlocks
