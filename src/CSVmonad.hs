{-# LANGUAGE Strict, OverloadedStrings #-}
module CSVmonad (
    CSVmonad
  , fromColumn
  , fromColumn'
  , usingMonad
  , usingMonadStat
  , (.=)
  , (..=)
  , output
  , outputStat
  , output'
  ) where

import qualified Data.ByteString as B
import qualified Data.Vector as T
import Data.Csv hiding ((.=))
import ShowByteString
import qualified Data.HashMap.Strict as M
import Control.Monad.Reader
import Control.Applicative

type CSVmonad a = ReaderT NamedRecord Parser a


fromColumn' :: FromField a => B.ByteString -> CSVmonad a
fromColumn' s = ReaderT (.: s)

fromColumn :: Field -> CSVmonad Field
fromColumn s = fromColumn' s <|> pure ""


type Head = Field

(.=) :: ByteStringable b => Field -> b -> (Head, Field)
s .= x = (s, toByteString x)


(..=) :: ByteStringable b => Field -> CSVmonad b -> CSVmonad (Head, Field)
s ..= x = fmap (s .=) x


output :: [(Head, Field)] -> CSVmonad ([Head], [Field])
output x = return $ unzip x 

output' :: [CSVmonad (Head, Field)] -> CSVmonad ([Head], [Field])
output' x = sequence x >>= output

outputStat :: a -> [(Head, Field)] -> CSVmonad (([Head], [Field]), a)
outputStat s x = return $ (unzip x, s)


usingMonad :: CSVmonad ([Head], [Field]) -> ([Head], NamedRecord -> [Field])
usingMonad x = change (getRight . runParser . runReaderT x)
  where change f = (fst (f M.empty), snd . f)

usingMonadStat :: CSVmonad (([Head], [Field]), a) -> (([Head], NamedRecord -> [Field]), CSVmonad a)
usingMonadStat x = change (getRight . runParser . runReaderT x)
  where change f = ((fst (fst $ f M.empty), snd . fst . f), snd <$> x)




getRight (Right x) = x
getRight (Left e) = error e
