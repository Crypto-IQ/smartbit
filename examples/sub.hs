module Main where

import Control.Monad
import Control.Monad.IO.Class
import Smartbit.API

-----------------------------------------------------------------------------

main :: IO ()
main = runSubscription $ do
  subNewTxns
  replicateM_ 5 recv'
  unsubNewTxns
  recv'
 where
  recv' = recv >>= print'
  print' = liftIO . print