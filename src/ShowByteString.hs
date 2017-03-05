{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ShowByteString (ByteStringable (..)) where

import Data.ByteString as B
import Data.ByteString.Char8 as C
import Data.String
import Data.Csv
import Data.Monoid (Sum)

class Show a => ByteStringable a where
  toByteString :: a -> ByteString
  toByteString = fromString . show

instance ByteStringable String where
  toByteString = C.pack

instance ByteStringable ByteString where
  toByteString = id

instance ByteStringable Double
instance ByteStringable Int
instance ByteStringable a => ByteStringable (Maybe a)
instance (ByteStringable a, ByteStringable b) => ByteStringable (a, b)
instance (ByteStringable a, ByteStringable b) => ByteStringable (Either a b)
instance ByteStringable a => ByteStringable (Sum a)
