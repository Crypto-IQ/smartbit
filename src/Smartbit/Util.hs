{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Smartbit.Util where

import           Data.Aeson.Casing.Internal
import           Data.Char
import           Data.Text                      (Text)
import qualified Data.Text                 as T (intercalate,pack,unpack)
import           Servant.Common.Text            (ToText(..))

-----------------------------------------------------------------------------

hyphenCase :: String -> String
hyphenCase = u . applyFirst toLower
 where
  u []                 = []
  u (x:xs) | isUpper x = '-' : toLower x : hyphenCase xs
           | otherwise = x : u xs

-----------------------------------------------------------------------------

type family HElem (r :: k) (rs :: [k]) :: Bool where
  HElem r '[] = 'False
  HElem r (r ': rs) = 'True
  HElem r (x ': rs) = HElem r rs

-----------------------------------------------------------------------------

data Select (ts :: [*]) where
  Item :: (Show t,HElem t ts ~ 'True) => t -> Select ts

instance Show (Select a) where
   show (Item x) = show x

instance ToText (Select a) where
  toText = T.pack . show
