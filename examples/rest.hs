{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Base58String.Bitcoin
import Smartbit.API

main :: IO ()
main = do
  as <- runSmartbit $ address' $ Addresses $ map fromText 
          [ "1BvvRfz4XnxSWJ524TusetYKrtZnAbgV3r"
          , "1dice8EMZmqKvrGE4Qc9bUFf9PX3xaYDp"]
  print as