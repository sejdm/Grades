module CassavaBackendMap (simple) where
 
import CSVextrasMap
import qualified Data.Vector as T hiding (sequence)
import qualified Data.ByteString.Lazy as B
import Data.Csv hiding (Parser)
import Statistics.Sample
import System.Environment
import Data.List
import qualified Data.ByteString.Internal as IB
import qualified Data.Map.Strict as M



combinedEither bs = mconcat <$>  (sequence $ map (fmap snd . decodeByName) bs)

combinedEitherIO ns = combinedEither <$> mapM B.readFile ns

eitherPrint (Right x) = print x
eitherPrint (Left x) = print x

roundTo n f = fromInteger ( round $ f * (10^n)) / (10.0^^n)

eitherPrintDouble (Right x) = print $ roundTo 1 x
eitherPrintDouble (Left x) = print x


simple x l = do
  ns <- getArgs
  es <- combinedEitherIO ns
  case es of
    Left e -> print e
    Right y -> do B.writeFile "out.csv" (encode $ T.toList $ uncurry createSheet x y)
--(encodeByName (T.fromList h) $ T.toList $ T.map (getMapFunc h f) y)
                  putStrLn "The Mean"
                  eitherPrintDouble $ stats mean l y
                  putStrLn ""
                  putStrLn "The Standard deviation"
                  eitherPrintDouble $ stats stdDev l y
                  putStrLn ""
                  putStrLn "The median"
                  eitherPrintDouble $ stats median l y
                  histOutput 10 "histogram.png" l y


median :: T.Vector Double -> Double
median xs = sort (T.toList xs) !! n
  where n = T.length xs `div` 2
