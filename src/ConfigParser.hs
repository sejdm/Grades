{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module ConfigParser (simpleMain) where


import Data.Yaml hiding ((.=))
import GHC.Generics
import qualified Data.Map as M
import Control.Applicative
import Grade
import ShowByteString
import CSVextrasMap
import CassavaBackendMap


data GradeType = 
  GradeGiven {called :: String, outof :: Double, absent :: String} 
  | Agg (M.Map String Double) 
  | Glist {prefixed :: String, eachoutof :: Double, absents :: String, take :: Int} 
  | Gavg String Int String 
  | Gfrom String String Double 
  | Glet String (M.Map String Double)  deriving (Show)



instance FromJSON GradeType where
  parseJSON (Object v) =  parseUsual v <|> parseGlist v <|> parseAgg v <|>  parseGavg v <|> parseGfrom v <|> parseScheme v

    where 
      parseUsual v  = GradeGiven <$> v .: "called" <*> v .: "outof" <*> parseAbsent v

      parseGlist v = Glist  <$> v .: "called" <*> v .: "eachoutof" <*> parseAbsent v <*> parseTake v

      parseAgg v =  Agg <$> v .: "aggregate"

      parseGavg v = Gavg <$> v .: "average" <*> parseDrop v <*> parseAbsent v

      parseGfrom v = Gfrom <$> v .: "from" <*> parseAbsent v <*> parseTo v

      parseScheme v = Glet <$> v .: "using" <*> v .: "scheme"

parseAbsent v = (v .: "absent") <|> pure "nochange"      
parseTake v = (v .: "take") <|> pure 0
parseDrop v = (v .: "drop") <|> pure 0
parseTo v = (v .: "take") <|> pure 0

data Out a = NumG {unNumG :: a -> Grade} | NumGs {unNumGs :: a -> [Grade]} | LetterG {unLetterG :: a -> LGrade}

whenAbs m a | a == "zero" = setZero 
            | otherwise = use (unNumG (changeTo m (unsafeLookUp a m)))

absChange m a = if a == "nochange" then id else flip ifAbsent (whenAbs m a)
absChanges m a = if a == "nochange" then id else flip forAnyAbsent (whenAbs m a)



changeTo m (GradeGiven s o a) = NumG (absChange m a (toByteString s `outOf` o))
changeTo m (Glist s o a t) = NumGs (absChanges m a (Prelude.take t $$ toByteString s `eachOutOf` o))
changeTo m (Gavg x d a) = NumG (combineButDrop d . absChanges m a ( unNumGs (changeTo m (unsafeLookUp x m))))
changeTo m (Agg xs) =  NumG (combine $ map f $ M.toList xs)
          where f (s, w) =  unNumG (changeTo m (unsafeLookUp s m)) *. w
changeTo m (Gfrom s a n) = case xx of 
    NumG r -> NumG (absChange m a $ if n /= 0 then r *. n else r)
    NumGs r -> NumGs (absChanges m a $ r)
  where xx = changeTo m $ unsafeLookUp s m
changeTo m (Glet s m') = LetterG (letterGradeFrom g $ unNumG (changeTo m (unsafeLookUp s m)))
  where g = lttr m'

changeTo'' m z y = case (changeTo m y) of
  NumG k -> z .= k
  LetterG k -> z .= k
  

lttr m x | x >= unsafeLookUp "O" m = O
         | x >= unsafeLookUp "A" m = A
         | x >= unsafeLookUp "B" m = B
         | x >= unsafeLookUp "C" m = C
         | x >= unsafeLookUp "D" m = D
         | x >= unsafeLookUp "E" m = E
         | otherwise = F



data Out' = Defined String String | Original String String deriving (Show, Generic)

instance FromJSON Out' where
  parseJSON (Object v) = (Defined <$> v .: "called" <*> v .: "computed") <|> (Original <$> v .: "called" <*> v .: "original")

changeToOut m (Defined s1 s2) = changeTo'' m (toByteString s1) (unsafeLookUp s2 m)
changeToOut m (Original s1 s2) = (toByteString s1) .= fromColumn (toByteString s2)

final m = usingList . map (changeToOut m)



nf = error "Not found the map!!!"
unsafeLookUp = M.findWithDefault nf

data Exam = Exam {exams :: M.Map String GradeType} deriving (Show, Generic, FromJSON)
data Output = Output {output :: [Out']} deriving (Show, Generic, FromJSON)
data Using = Using {using :: String} deriving (Show, Generic, FromJSON)

unJust (Right x) = x
unJust (Left e)= error (show e)

stngfl=  "gradingscheme.yml"

simpleMain = do x <-  exams . unJust <$> decodeFileEither stngfl
                y <-  output . unJust <$> decodeFileEither stngfl
                u <-  using . unJust <$> decodeFileEither stngfl
                let s = final x y
                simple s (unNumG $ changeTo x (unsafeLookUp u x))
                
