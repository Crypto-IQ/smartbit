{-# LANGUAGE OverloadedStrings #-}

module Main where

import Smartbit.API

-----------------------------------------------------------------------------

main :: IO ()
main = do
  let a1 = Addresses $ map fromText ["1BvvRfz4XnxSWJ524TusetYKrtZnAbgV3r"]
      a2 = Addresses $ map fromText [ "1BvvRfz4XnxSWJ524TusetYKrtZnAbgV3r"
                                    , "1dice8EMZmqKvrGE4Qc9bUFf9PX3xaYDp"]
  print =<< runSmartbit (address'   a1)
  print =<< runSmartbit (address'   a2)
  print =<< runSmartbit (address    a1
                                    (Just TxnFilter'OpReturns)
                                    Nothing
                                    (Just (Item (SortBy :: SortBy TxIndex')))
                                    Nothing
                                    Nothing
                                    Nothing)
  print =<< runSmartbit  addresses'
  print =<< runSmartbit (addresses  (Just 5)
                                    (Just (Item (SortBy :: SortBy Balance')))
                                    (Just SortDir'Asc)
                                    Nothing
                                    Nothing)

