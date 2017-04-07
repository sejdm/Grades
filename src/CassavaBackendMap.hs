{-# LANGUAGE Strict, NoMonomorphismRestriction #-}
module CassavaBackendMap (simple, simpleStat, simpleM) where
 
import qualified Data.Vector as T hiding (sequence)
import qualified Data.ByteString.Lazy as B
import Data.Csv hiding (Parser)
import System.Environment
import qualified Data.Map.Strict as M
import GradeParser


createSheet (h, f) = T.cons (T.fromList h) . T.map (T.fromList . f) 

combinedEither bs = mconcat <$>  (mapM (fmap snd . decodeByName) bs)

combinedEitherIO ns = combinedEither <$> mapM B.readFile ns


finalIO m x l = do 
  ns <- getArgs
  es <- combinedEitherIO ns
  case es of
    Left e -> print e
    Right y -> do B.writeFile "out.csv" (encode $ T.toList $ createSheet x y)
                  m l y

simple = flip (finalIO (\_ _ -> return ())) ()

simpleM = finalIO printStats  
simpleStat = uncurry simpleM
