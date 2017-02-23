{-# LANGUAGE OverloadedStrings #-}
 
module CSVextras (
    setColumn
  , outOf
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

import Data.Vector as T hiding (sequence)
import Data.ByteString as B
import Data.ByteString.Char8 as C
import Safe
import Grade
import Histogram
import Data.Csv hiding ((.=))
import Control.Applicative
import Data.String
import ShowByteString

getIndex :: B.ByteString -> B.ByteString -> Vector ByteString -> Int
getIndex e x v = case T.toList (T.elemIndices x v) of
             [] -> error ("No fromColumn labelled " Prelude.++ show e)
             (x:_) -> x

getIndicesPrefixed :: B.ByteString -> Vector ByteString -> Vector Int
getIndicesPrefixed p = T.findIndices (B.isPrefixOf p)


--parseCell :: Read a => ByteString -> Maybe a
parseCell s = runParser $ parseField s

--outOf :: Double -> V.Vector B.ByteString -> B.ByteString -> V.Vector B.ByteString -> Grade
outOf c o h r =  either (const (absent o)) (/.o) $ parseCell $ r T.! (getIndex c c h)
outOfs cs o h r =  either (const (absent o)) (/.o) $ Prelude.foldl1 (<|>) $ Prelude.map (\c -> parseCell $ r T.! (getIndex c c h)) cs

(./) = flip outOf
(.//) = flip outOfs

eachOutOf c o h r = T.toList $ T.map (either (const (absent o)) (/.o)  . parseCell . ( r T.!)) $ getIndicesPrefixed c h
eachOutOfs cs o h r = T.toList $ T.map (either (const (absent o)) (/.o)  . parseCell . ( r T.!)) $ Prelude.foldl1 (<|>) $ Prelude.map ( \c ->getIndicesPrefixed c h) cs

fromColumn c h r = r T.! (getIndex c c h)


--setColumn :: Show a => String -> (B.ByteString -> V.Vector B.ByteString -> a) -> (String, (B.ByteString -> V.Vector B.ByteString -> B.ByteString))
setColumn :: ByteStringable a => t1 -> (t -> a1 -> a) -> [(t1, t -> a1 -> ByteString)]
setColumn s f = [(s, \x -> toByteString . f x)]

(.=) :: ByteStringable a => t1 -> (t -> a1 -> a) -> [(t1, t -> a1 -> ByteString)]
(.=) = setColumn


--fromList :: [(String, (B.ByteString -> V.Vector B.ByteString -> B.ByteString))] -> ([String], B.ByteString -> V.Vector B.ByteString -> [B.ByteString])
usingList x = (s , sequence . sequence fs)
  where (s, fs) = T.unzip  $ T.fromList $ Prelude.concat x

changeAll (h, c) xs = T.cons h $ T.map (c (T.head xs)) (T.tail xs)



stats f l v = (f<$>) . sequence . T.filter isNotAbsent . fmap (marksEither . l (T.head v)) $ T.tail v
  where isNotAbsent (Left (Absent _)) = False
        isNotAbsent _ = True



histOutput m s l n = case (stats (histogram (assignBin m) . T.toList) l n) of
                        Right a -> histToChart s a
                        Left e -> print e
