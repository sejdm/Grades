module CassavaBackend (simple) where
 
import CSVextras
import Data.Vector as T hiding (sequence)
import qualified Data.ByteString.Lazy as B
import Data.Csv hiding (Parser)
import Statistics.Sample
import System.Environment
import Data.List
import qualified Data.Map as M


encdec x b =encode . T.toList . changeAll x <$> decode NoHeader b

simple x l = do
  (n:o:_) <- getArgs
  b <- B.readFile n
  case encdec x b of
    Left e -> print e
    Right x -> B.writeFile o x
  case decode NoHeader b of
    Left e -> print e
    Right x -> do histOutput 10 "histogram.png" l x
                  printStats l x

printStats l x = do putStrLn "Mean"
                    putStrLn $ either show show (stats mean l x)
                    putStrLn ""
                    putStrLn "Standard deviation"
                    putStrLn $ either show show  (stats stdDev l x)
                    putStrLn ""
                    putStrLn "Median"
                    putStrLn $ either show show  (stats median l x)


median :: T.Vector Double -> Double
median xs = sort (T.toList xs) !! n
  where n = T.length xs `div` 2
