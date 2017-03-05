{-# LANGUAGE OverloadedStrings #-}
module CSVextras (
    outOf
  , outOfs
  , eachOutOf
  , eachOutOfs
  , fromColumn
  , usingList
  , changeAll
  , stats
  , histOutput
  , (.=)
  ) where

import qualified Data.Vector as T hiding (sequence)
import qualified Data.ByteString as B
import Safe
import Grade
import Histogram
import Data.Csv hiding ((.=))
import Control.Applicative
import Data.String
import ShowByteString

getIndex :: B.ByteString -> B.ByteString -> T.Vector B.ByteString -> Int
getIndex e x v = case T.toList (T.elemIndices x v) of
             [] -> error ("No fromColumn labelled " Prelude.++ show e)
             (x:_) -> x

getIndicesPrefixed :: B.ByteString -> T.Vector B.ByteString -> T.Vector Int
getIndicesPrefixed p = T.findIndices (B.isPrefixOf p)


parseCell :: FromField a => Field -> Either String a
parseCell s = runParser $ parseField s

outOf :: B.ByteString -> Double -> T.Vector B.ByteString -> T.Vector Field -> Grade
outOf c o h r =  either (const (absent o)) (/.o) $ parseCell $ r T.! getIndex c c h

outOfs :: [B.ByteString] -> Double -> T.Vector B.ByteString -> T.Vector Field -> Grade
outOfs cs o h r =  either (const (absent o)) (/.o) $ Prelude.foldl1 (<|>) $ Prelude.map (\c -> parseCell $ r T.! getIndex c c h) cs

(./) = flip outOf
(.//) = flip outOfs

eachOutOf :: B.ByteString -> Double -> T.Vector B.ByteString -> T.Vector Field -> [Grade]
eachOutOf c o h r = T.toList $ T.map (either (const (absent o)) (/.o)  . parseCell . ( r T.!)) $ getIndicesPrefixed c h

eachOutOfs :: [B.ByteString] -> Double -> T.Vector B.ByteString -> T.Vector Field -> [Grade]
eachOutOfs cs o h r = T.toList $ T.map (either (const (absent o)) (/.o)  . parseCell . ( r T.!)) $ Prelude.foldl1 (<|>) $ Prelude.map ( `getIndicesPrefixed` h) cs

fromColumn :: B.ByteString -> T.Vector B.ByteString -> T.Vector a -> a
fromColumn c h r = r T.! getIndex c c h


s .= f = (s, \x -> toByteString . f x)



usingList :: [ (a1, b -> a)] -> (T.Vector a1, b -> (T.Vector a))
usingList x = (s , sequence fs)
  where (s, fs) = T.unzip  $ T.fromList x


changeAll :: (a, a1 -> a1 -> a) -> T.Vector a1 -> T.Vector a
changeAll (h, c) xs = T.cons h $ T.map (c (T.head xs)) (T.tail xs)



stats :: (T.Vector Double -> b) -> (a -> a -> Grade) -> T.Vector a -> Either GradeErrors b
stats f l v = (f<$>) . sequence . T.filter isNotAbsent . fmap (marksEither . l (T.head v)) $ T.tail v
  where isNotAbsent (Left (Absent _)) = False
        isNotAbsent _ = True



histOutput :: Double -> FilePath -> (a -> a -> Grade) -> T.Vector a -> IO ()
histOutput m s l n = case stats (histogram (assignBin m) . T.toList) l n of
                        Right a -> histToChart s a
                        Left e -> print e
