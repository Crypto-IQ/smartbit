{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Smartbit.Util where

import           Data.Aeson.Casing.Internal
import           Data.Char
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
