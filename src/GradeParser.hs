{-# LANGUAGE Strict, OverloadedStrings #-}
module GradeParser (
    outOf
  , eachOutOf
  , stats
  , histOutput
  , printStats
  ) where

import qualified Data.ByteString as B
import Grade
import qualified Data.Vector as T
import Data.Csv hiding ((.=))
import ShowByteString
import qualified Data.HashMap.Strict as M
import Control.Monad.Reader
import Control.Applicative
import Histogram
import Data.List
import Statistics.Sample
import CSVmonad


outOf :: Field -> Double -> CSVmonad Grade
outOf s o = ReaderT (\v -> ((/. o) <$> v .: s) <|> pure (absent o))

parseGrade o g = ((/.o) <$> parseField g) <|> pure (absent o)

eachOutOf s o = ReaderT (\v -> sequence (map (parseGrade o) $ M.elems $ M.filterWithKey (\k _ -> B.isPrefixOf s k) v))


getRight (Right x) = x
getRight (Left e) = error $ show e



stats :: (T.Vector Double -> b) -> CSVmonad Grade -> T.Vector NamedRecord -> Either GradeErrors b
stats f l v = fmap f $ sequence $ T.filter isNotAbsent $ fmap (marksEither . getRight . runParser .  runReaderT l ) v
  where isNotAbsent (Left (Absent _)) = False
        isNotAbsent _ = True

histOutput :: Double -> FilePath -> CSVmonad Grade -> T.Vector NamedRecord -> IO ()
histOutput m s l v = case stats (histogram (assignBin m) . T.toList) l v of
                        Right a -> histToChart s a
                        Left e -> print e


eitherPrintDouble (Right x) = print $ roundTo 1 x
eitherPrintDouble (Left x) = print x

printStat n s l y = putStrLn n >> eitherPrintDouble (stats s l y) >> putStrLn ""

--printStats l y = do printStat "The Mean" mean l y
--                    printStat "The Standard deviation" stdDev l y
--                    printStat "The Median" median l y
--                    histOutput 10 "histogram.png" l y
--
printStats l y = do let m = getRight $ stats mean l y
                    let s = getRight $ stats stdDev l y
                    let me = getRight $ stats median l y
                    printStat "The Mean" mean l y
                    printStat "The Standard deviation" stdDev l y
                    printStat "The Median" median l y
                    histOutput 10 "histogram.png" l y


round' x | f < 0.5 = i
         | otherwise = i + 1
         where i = floor x
               f = x - fromIntegral i

roundTo n f = fromInteger (round' $ f * (10^n)) / (10.0^^n)


median :: T.Vector Double -> Double
median xs = sort (T.toList xs) !! n
  where n = T.length xs `div` 2
