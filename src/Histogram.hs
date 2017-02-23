{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
 
module Histogram (
  Histogram
  , updateHist
  , histogram
  , toList
  , fromList
  , histList
  , histToChart
  , chartHist
  , toMap
  , fromMap
  , histTotal
  , histSum
  , histMean
  , histStdDev
  , assignBin
  ) where

import qualified Data.Map.Strict as M
import Data.List hiding (sum)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Control.DeepSeq
import ConstrainedStuff as C hiding ((<$>))
import Prelude hiding (sum, Functor (..))

newtype Histogram a = Histogram {toMap :: M.Map a Int} deriving (Show, NFData)

fromMap :: M.Map a Int -> Histogram a
fromMap = Histogram

instance Ord a => Monoid (Histogram a) where
  mappend = addHist
  mempty = empty
  mconcat = combineHists

instance Functor Histogram where
  type FunctorConstraint Histogram a = Ord a
  fmap f h = fromList $ map (\(k, n) -> (f k, n)) (toList h)

empty :: Histogram a
empty = fromMap M.empty

updateHist :: Ord a => (b -> a) -> Histogram a -> b -> Histogram a
updateHist f h x = fromMap $ M.insertWith (+) (f x) 1 (toMap h)

listToHist :: Ord a => (b -> a) -> Histogram a -> [b] -> Histogram a
listToHist f h = foldl' (updateHist f) h


histogram :: Ord a => (b -> a) -> [b] -> Histogram a
histogram f = listToHist f empty

toList :: Histogram a -> [(a, Int)]
toList = M.toList . toMap

fromList :: Ord a => [(a, Int)] -> Histogram a
fromList = fromMap . M.fromList

histList :: Ord a => (b -> a) -> [b] -> [(a, Int)]
histList f = toList . histogram f

addHist :: Ord a => Histogram a -> Histogram a -> Histogram a
addHist x y = fromMap (M.unionWith (+) (toMap x) (toMap y))

combineHists :: Ord a => [Histogram a] -> Histogram a
combineHists = foldl' addHist empty
--combineHists = foldl' (M.unionWith (+)) M.empty


chartHist n f vs = toFile def n $ do
     layout_title .= "Sample Bars"
     layout_title_style . font_size .= 10
     layout_x_axis . laxis_generate .= autoIndexAxis (map (show . fst) values)
     plot $ fmap plotBars $ bars [""] (addIndexes (map (return . snd ) values))
     where values = histList f vs


histToChart n h = toFile def n $ do
     layout_title .= "Sample Bars"
     layout_title_style . font_size .= 10
     layout_x_axis . laxis_generate .= autoIndexAxis (map (show . fst) values)
     plot $ fmap plotBars $ bars [""] (addIndexes (map (return . snd ) values))
     where values = toList h

histTotal :: Histogram a -> Int
histTotal = sum . map snd . toList

histSum :: (Real n, Real m, Fractional m) => Histogram n -> m
histSum = sum . map f . toList
  where f (x, n) = realToFrac x * fromIntegral n

histMean :: (Real n, Fractional m, Real m) => Histogram n -> m
histMean = (/) <$> histSum <*> fromIntegral . histTotal
--histMean h = realToFrac s / realToFrac t
  --where f !(!x, !n) !(!x', !n') = ((fromIntegral n) * x + (fromIntegral n') * x', n+n')
        --(s, t) = foldl' f  (0,0) (toList h)

histVariation :: (Real n, Fractional m, Real m) => Histogram n -> m
histVariation h = (avg . map f . toList) h
  where f (x, n) = fromIntegral n * (realToFrac x - m)^2
        m = histMean h

histStdDev :: (Real n, Fractional m, Real m, Floating m) => Histogram n -> m
histStdDev = sqrt . histVariation

sum :: Real n => [n] -> n
sum = foldl1' (+)



avg :: (Real a, Fractional b) => [a] -> b
avg xs = realToFrac s' / realToFrac n'
  where (s', n') = foldl' (\ !(!s, !n) x -> (s+x, n+1)) (0, 0) xs


newtype RangeBin n = RangeBin (n, n) deriving (Eq)

instance (Ord n) => Ord (RangeBin n) where
  compare (RangeBin (m,_)) (RangeBin (n,_ )) = compare m n

instance Show n => Show (RangeBin n) where
  show (RangeBin (m, n)) = show m ++ "-" ++ show n

assignBin :: Double -> Double -> RangeBin Double
assignBin n x = RangeBin (a*n, a*n +n)
  where a = fromIntegral $ floor (x/n)


roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

test = histogram (assignBin 10) [1,2,3]
