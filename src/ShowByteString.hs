{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ShowByteString (ByteStringable (..)) where

import Data.ByteString as B
import Data.ByteString.Char8 as C
import Data.String
import Data.Csv

class Show a => ByteStringable a where
  toByteString :: a -> ByteString
  toByteString = fromString . show

instance ByteStringable String where
  toByteString = C.pack

instance ByteStringable ByteString where
  toByteString = id
