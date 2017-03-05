{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts, NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}

module Grade
    (
      Grade
    , GradeErrors (..)
    , LetterGrade (..)
    , absent
    , LGrade
    , unparsed
    , marksEither

    , (/.)
    , (*.)
    , (+.)
    , ifAbsent
    , forAnyAbsent
    , ($$)
    , ($$$)
    , ($$$$)

    , use
    , setZero
    , dropGrades
    , combine
    , combineButDrop
    , letterGradeFrom

    , indexed
    ) where

import Data.Monoid
import Control.DeepSeq
import Control.Applicative
import Data.Csv
import Data.List
import GHC.Generics hiding (to, from)
import ShowByteString
import SemiVectorSpace
import Safe

data GradeErrors = Uncomputed | Unparsed | Unknown String | Absent Double deriving (Eq, Ord, Generic, NFData, ByteStringable)

instance Show GradeErrors where
  show (Unknown s) = "No grade called " ++ s
  show Uncomputed = "The grade was uncomputed"
  show Unparsed = "The grade was unparsed"
  show (Absent _) = "Ab"

newtype Grade = Grade {unGrade :: Either GradeErrors (Sum Double, Sum Double)} deriving (Eq, Generic, NFData, ByteStringable)


marksEither :: Grade -> Either GradeErrors Double
marksEither x = getSum . fst <$> unGrade x

outofEither :: Grade -> Either GradeErrors Double
outofEither x = getSum . snd <$> unGrade x

-- Grade constructor
x /. y = Grade $ pure (Sum x, Sum y)

instance Show Grade where
  show (Grade (Right (Sum x, Sum 100))) = show (roundTo 1 x)
  show (Grade (Right (Sum x, Sum y))) = show (roundTo 1 x)
  show (Grade (Left e)) = show e

instance Ord Grade where
  compare (Grade (Right (Sum x, Sum y))) (Grade (Right (Sum x', Sum y'))) = compare (x/y) (x'/y')
  compare (Grade (Right (Sum _, Sum _))) (Grade (Left _)) = GT
  compare (Grade (Left _)) (Grade (Right (Sum _, Sum _))) = LT
  compare (Grade (Left e)) (Grade (Left e')) = compare e e'

instance Monoid Grade where
  mappend x y = Grade $  liftA2 (<>) (unGrade x) (unGrade y)
  mempty = Grade (Right (Sum 0, Sum 0))

instance SemiVectorSpace Grade where
  t #* x = Grade $ normalize t $ unGrade x
     where normalize z = fmap (\(Sum a, Sum b) -> (Sum (a/b*z), Sum z))

gradeError :: GradeErrors -> Grade
gradeError = Grade . Left

unparsed :: Grade
unparsed = gradeError Unparsed

absent :: Double -> Grade
absent = gradeError . Absent

unknown :: String -> Grade
unknown = gradeError . Unknown


  
(+.) = (<>)

(*.) = flip (#*)


infixl 6 +.
infixl 7 *.



-- Letter Grades
data LetterGrade = O | Ap | A | Am | Bp | B | Bm | Cp | C | Cm | Dp | D | Dm | Ep | E | Em | F | I deriving (Eq, Ord, ByteStringable)

instance Show LetterGrade where
  show l = case l of
    O -> "O"
    A -> "A"
    B -> "B"
    C -> "C"
    D -> "D"
    E -> "E"
    F -> "F"
    Ap -> "A+"
    Bp -> "B+"
    Cp -> "C+"
    Dp -> "D+"
    Ep -> "E+"
    Am -> "A-"
    Bm -> "B-"
    Cm -> "C-"
    Dm -> "D-"
    Em -> "E-"
    I -> "I"


newtype LGrade = LGrade (Either GradeErrors LetterGrade) deriving (ByteStringable)



instance Show LGrade where
   show (LGrade (Right g)) = show g
   show (LGrade (Left (Absent _))) = show I
   show (LGrade (Left e)) = show e


roundTo :: (RealFrac a1, Fractional a, Integral b) => b -> a1 -> a
--roundTo n f = fromInteger ( round $ f * (10^n)) / (10.0^^n)
roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)





ifAbsent :: (t -> Grade) -> (Double -> t -> Grade) -> t -> Grade
ifAbsent l f r = case l r of 
                      Grade (Left ( Absent n)) -> f n r
                      y -> y


forAnyAbsent :: (t -> [Grade]) -> (Double -> t -> Grade) -> t -> [Grade]
forAnyAbsent l f r = map g (l r)
  where g (Grade (Left ( Absent n))) = f n r
        g y = y

use :: t1 -> t -> t1
use l _  = l

setZero :: Double -> t -> Grade
setZero n _  = 0 /. n


infixl 8 `ifAbsent`
infixl 8 `forAnyAbsent`


($$) = fmap
($$$) = fmap . fmap
($$$$) = fmap . fmap . fmap

infixr 0 $$
infixr 0 $$$
infixr 0 $$$$


indexed l n = ind (n-1) $$ l
  where ind n xs = case atMay xs n of
                      Just t -> t
                      _ -> unparsed


dropGrades :: Ord a => Int -> [a] -> [a]
dropGrades n = drop n . sort

combineButDrop :: (Ord c, Monoid c) => Int -> [c] -> c
combineButDrop n = mconcat . dropGrades n

combine :: Monoid a => [a] -> a
combine = mconcat


letterGradeFrom :: (Double -> LetterGrade) -> (a -> Grade) -> a -> LGrade
letterGradeFrom f l =  LGrade . (f <$>) . marksEither . l
