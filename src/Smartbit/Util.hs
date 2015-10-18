{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smartbit.Util where

import           Data.Aeson.Casing.Internal
import qualified Data.Base58String.Bitcoin as B
import           Data.Char
import           Data.String
import qualified Data.Text                 as T (pack)
import           Servant.Common.Text            (ToText(..))

-----------------------------------------------------------------------------

hyphenCase :: String -> String
hyphenCase = u . applyFirst toLower
 where
  u []                 = []
  u (x:xs) | isUpper x = '-' : toLower x : hyphenCase xs
           | otherwise = x : u xs

-----------------------------------------------------------------------------

instance IsString B.Base58String where
  fromString = B.fromText . T.pack 

-----------------------------------------------------------------------------

type family Elem (r :: k) (rs :: [k]) :: Bool where
  Elem r '[] = 'False
  Elem r (r ': rs) = 'True
  Elem r (x ': rs) = Elem r rs

-----------------------------------------------------------------------------

data Select (ts :: [*]) where
  Item :: (Show t,Elem t ts ~ 'True) => t -> Select ts

instance Show (Select a) where
   show (Item x) = show x

instance ToText (Select a) where
  toText = T.pack . show
