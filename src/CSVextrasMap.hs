{-# LANGUAGE OverloadedStrings #-}
module CSVextrasMap (
    outOf
  , outOfs
  , eachOutOf
  , eachOutOfs
  , fromColumn
  , usingList
  , createSheet
  , stats
  , histOutput
  , (.=)
  , CSVHeader
  , CSVRecord
  , CSVField
  , MyNamedRecord
  , getMapFunc
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
import qualified Data.Map.Strict as M

type MyMap = M.Map
type MyNamedRecord = MyMap B.ByteString Field
defaultFind = M.findWithDefault

parseCell :: FromField a => Field -> Either String a
parseCell s = runParser $ parseField s

outOf :: B.ByteString -> Double -> MyMap B.ByteString Field -> Grade
outOf c o r =  either (const (absent o)) (/.o) $ parseCell $ defaultFind "" c r

outOfs :: [B.ByteString] -> Double -> MyMap B.ByteString Field -> Grade
outOfs cs o r =  either (const (absent o)) (/.o) $ Prelude.foldl1 (<|>) $ Prelude.map (\c -> parseCell $ defaultFind "" c r) cs

eachOutOf :: B.ByteString -> Double -> MyMap B.ByteString Field -> [Grade]
eachOutOf c o r = Prelude.map (either (const (absent o)) (/.o)  . parseCell) $ M.elems $ M.filterWithKey (\k _ -> B.isPrefixOf c k) r

eachOutOfs :: [B.ByteString] -> Double -> MyMap B.ByteString Field -> [Grade]
eachOutOfs cs o r = Prelude.map (either (const (absent o)) (/.o)  . parseCell) $ Prelude.foldl1 (<|>) (Prelude.map (\c -> (M.elems $ M.filterWithKey (\k _ -> B.isPrefixOf c k) r)) cs)

fromColumn :: B.ByteString -> MyMap B.ByteString Field -> Field
fromColumn c r = defaultFind "" c r

type CSVHeader = T.Vector B.ByteString
type CSVField = B.ByteString
type CSVRecord = T.Vector CSVField
type CSVSheet = T.Vector CSVRecord

(.=) :: ByteStringable a => B.ByteString -> (t -> a) -> (B.ByteString, t -> CSVField)
s .= f = (s, toByteString . f)


usingList :: [(B.ByteString, t1 -> CSVField)] -> ([Field], t1 -> [Field])
usingList x = (s , sequence fs)
  where (s, fs) = unzip  x


toVectorStyle (h, f) = (T.fromList h, T.fromList . f)
toMapStyle (h, f) = (h, M.fromList . zip h . f)

getMapFunc h f = M.fromList . zip h . f

createSheet :: [a] -> (a1 -> [a]) -> T.Vector a1 -> T.Vector (T.Vector a)
createSheet h f = T.cons (T.fromList h) . T.map (T.fromList . f) 



stats :: (T.Vector Double -> b) -> (a -> Grade) -> T.Vector a -> Either GradeErrors b
stats f l v = fmap f $ sequence $ T.filter isNotAbsent $ fmap (marksEither . l ) v
  where isNotAbsent (Left (Absent _)) = False
        isNotAbsent _ = True

histOutput :: Double -> FilePath -> (a -> Grade) -> T.Vector a -> IO ()
histOutput m s l v = case stats (histogram (assignBin m) . T.toList) l v of
                        Right a -> histToChart s a
                        Left e -> print e
