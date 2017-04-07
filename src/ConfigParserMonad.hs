{-# LANGUAGE Strict, OverloadedStrings, DeriveGeneric, DeriveAnyClass, GADTs #-}
module ConfigParserMonad (simpleMain) where

import Data.Yaml hiding ((.=))
import GHC.Generics
import qualified Data.Map as M
import Control.Applicative
import Grade
import ShowByteString
import GradeParser
import CassavaBackendMap
import Control.Monad.Reader
import CSVmonad hiding (output)

type Env = M.Map String Var

data Var = NumV (Env -> CSVmonad Grade) 
         | NumVs (Env -> CSVmonad [Grade]) 
         | LetterV (Env -> CSVmonad LGrade)

type VarErr = String

getNum :: VarErr -> Env -> String -> CSVmonad Grade
getNum e m s = case unsafeLookUp s m of 
                 NumV z -> z m
                 _ -> error $ "This is not a single grade. " ++ e
                
getNums :: VarErr -> Env -> String -> CSVmonad [Grade]
getNums e m s = case unsafeLookUp s m of 
                   NumVs z -> z m
                   _ -> error $ "This is not a grade list. " ++ e
                
getLetter :: VarErr -> Env -> String -> CSVmonad LGrade
getLetter e m s = case unsafeLookUp s m of 
                     LetterV z -> z m
                     _ -> error $ "This is not a letter grade. " ++ e
               

abParse :: (a -> (Double -> CSVmonad Grade) -> a) -> Object -> Parser (Env -> a -> a)
abParse g v = (do a <- v .: "absent"
                  if a == ("zero" :: String) then return (\_ -> (flip g setZeroM)) 
                  else return (\m -> flip g (useM $ getNum "In absent" m a))) <|> pure (\_ -> id)
----
--
instance FromJSON Var where
  parseJSON (Object v) = parseFrom v <|> parseFroms v <|> parseUsual v  <|> parseList v <|> parseAvg v <|> parseAgg v <|> parseLetter v
    where 
       parseUsual v = do x <- v .: "called"
                         y <- v .: "outof"
                         g <- abParse ifAbsentM v
                         return $ NumV $ \m -> g m (toByteString (x :: String) `outOf` y)
--
       parseList v = do x <- v .: "called"
                        y <- v .: "eachoutof"
                        t <- (take <$> v .: "take") <|> pure id
                        g <- abParse forAnyAbsentM v
                        return $ NumVs $ \m ->  t <$>  g m (toByteString (x :: String) `eachOutOf` y)
  --
       parseAvg v = do s <- v .: "average"
                       d <- v .: "drop" <|> pure 0
                       return $ NumV $ \m ->  combineButDrop d <$> getNums "In average" m s
--
       parseAgg v = do m' <- v .: "aggregate"
                       let l = M.toList m'
                       let f n (s', w) = (*. w) <$> getNum "In aggregate" n s'
                       return $ NumV $ \m ->  mconcat <$> (mapM (f m) l)
--
       parseFrom v = do 
                        f <- v .: "from"
                        g <- (flip (*.) <$> v .: "to") <|> pure id
                        a <- abParse ifAbsentM v
                        return $ NumV $ \m -> g <$> a m (getNum "In from" m f )
--
       parseFroms v = do f <- v .: "from"
                         d <- v .: "drop" <|> pure 0
                         g <- abParse forAnyAbsentM v
                         return $ NumVs $ \m -> drop d <$> g m (getNums "In froms" m f)
--
       parseLetter v = do a <- v .: "using"
                          f <- v .: "scheme"
                          return $ LetterV $ \m -> letterGradeFromM (lttr f) (getNum "In letter" m a)
--


lttr m x | x >= unsafeLookUp "O" m = O
         | x >= unsafeLookUp "A" m = A
         | x >= unsafeLookUp "B" m = B
         | x >= unsafeLookUp "C" m = C
         | x >= unsafeLookUp "D" m = D
         | x >= unsafeLookUp "E" m = E
         | otherwise = F




changeTo''' m z (NumV f) = z ..= f m
changeTo''' m z (LetterV f) = z ..= f m
changeTo''' _ _ _ = error "cannot assign a column to a list"
--
--
--
data Out' = Defined String String | Original String String deriving (Show, Generic)


instance FromJSON Out' where
  parseJSON (Object v) = (Defined <$> v .: "called" <*> v .: "computed") <|> (Original <$> v .: "called" <*> v .: "original")



changeToOut m (Defined s1 s2) = changeTo''' m (toByteString s1) (unsafeLookUp' s2 m)
changeToOut m (Original s1 s2) = (toByteString s1) ..= fromColumn (toByteString s2)

final m = usingMonad . output' . map (changeToOut m)


nf = error "Unknown name "
unsafeLookUp = M.findWithDefault nf
--
unsafeLookUp' :: String -> M.Map String x -> x
unsafeLookUp' x = M.findWithDefault (error ("Undefined name: " ++ show x)) x
--
data Exam = Exam {exams :: M.Map String Var} deriving (Generic, FromJSON)
data Output = Output {output :: [Out']} deriving (Show, Generic, FromJSON)
data Using = Using {statistics :: String} deriving (Show, Generic, FromJSON)

unJust (Right x) = x
unJust (Left e)= error (show e)

stngfl=  "gradingscheme.yml"

simpleMain = do x <-  exams . unJust <$> decodeFileEither stngfl
                y <-  output . unJust <$> decodeFileEither stngfl
                u <-  statistics . unJust <$> decodeFileEither stngfl
                let s = final x y
                simpleM s ((chk $ (unsafeLookUp' u x)) x)
                    where chk (NumV x) = x
                          chk _ = error "You can perform statistics using only a non-list grade"
