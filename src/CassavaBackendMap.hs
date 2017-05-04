{-# LANGUAGE Strict, NoMonomorphismRestriction, MonadComprehensions #-}
module CassavaBackendMap (
  simple
  , simpleStat
  , simpleStatSafe
  , simpleStatSafeHist
  , simpleM
  , simpleMsafe
  , simpleMsafeHist
  ) where
 
import qualified Data.Vector as T hiding (sequence)
import qualified Data.ByteString.Lazy as B
import Data.Csv hiding (Parser)
import System.Environment
import qualified Data.Map.Strict as M
import GradeParser

rights :: Monad m => m (Either t b) -> m b
rights x = [a | Right a <- x]

createSheet :: ([a], a1 -> [a]) -> T.Vector a1 -> T.Vector (T.Vector a)
createSheet (h, f) = T.cons (T.fromList h) . T.map (T.fromList . f) 


createSheetSafe :: (a1 -> Either t [a], a1 -> Either t1 [a]) -> T.Vector a1 -> T.Vector (T.Vector a)
createSheetSafe (h, f) vs = T.cons (T.fromList (T.head $ rights $ T.map h vs)) $ rights $ T.map (fmap T.fromList . f) vs


combinedEither :: FromNamedRecord a => [B.ByteString] -> Either String (T.Vector a)
combinedEither bs = mconcat <$>  (mapM (fmap snd . decodeByName) bs)

combinedEitherIO :: FromNamedRecord a => [FilePath] -> IO (Either String (T.Vector a))
combinedEitherIO ns = combinedEither <$> mapM B.readFile ns


finalIO :: (FromNamedRecord a1, ToField a) => (t -> T.Vector a1 -> IO ()) -> ([a], a1 -> [a]) -> t -> IO ()
finalIO m x l = do 
  ns <- getArgs
  es <- combinedEitherIO ns
  case es of
    Left e -> print e
    Right y -> do B.writeFile "out.csv" (encode $ T.toList $ createSheet x y)
                  m l y


finalIOSafe m x l = do 
  ns <- getArgs
  es <- combinedEitherIO ns
  case es of
    Left e -> print e
    Right y -> do B.writeFile "out.csv" (encode $ T.toList $ createSheetSafe x y)
                  m l y

simple = flip (finalIO (\_ _ -> return ())) ()

simpleM = finalIO printStats  
simpleMsafe = finalIOSafe printStats  
simpleMsafeHist h = finalIOSafe (printStatsHist h)
simpleStat = uncurry simpleM
simpleStatSafe = uncurry simpleMsafe
simpleStatSafeHist = uncurry simpleMsafeHist
