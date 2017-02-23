{-# LANGUAGE   FlexibleInstances, DeriveGeneric, RankNTypes, FlexibleContexts, TemplateHaskell, NoMonomorphismRestriction, TypeFamilies, GeneralizedNewtypeDeriving #-}

module Grade
    (
      Grade
    , GradeErrors (..)
    , LetterGrade (..)
    , absent
    , unparsed
    , marksEither

    , (/.)
    , (*.)
    , (*..)
    , (+.)
    , (.*)
    , ifAbsent
    , forAnyAbsent
    , ($$)

    , use
    , setZero
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





data GradeErrors = Uncomputed | Unparsed | Unknown String | Absent Double deriving (Eq, Ord, Generic)

instance Show GradeErrors where
  show (Unknown s) = "No grade called " ++ s
  show Uncomputed = "The grade was uncomputed"
  show Unparsed = "The grade was unparsed"
  show (Absent _) = "Absent"

newtype Grade = Grade {unGrade :: (Either GradeErrors (Sum Double, Sum Double))} deriving (Eq, Generic)

instance NFData GradeErrors
instance NFData Grade
instance ByteStringable Grade
instance ByteStringable GradeErrors

marksEither :: Grade -> Either GradeErrors Double
marksEither x = getSum . fst <$> unGrade x

outofEither :: Grade -> Either GradeErrors Double
outofEither x = getSum . snd <$> unGrade x


-- Grade constructor
x /. y = Grade $ pure (Sum x, Sum y)

instance Show Grade where
  show (Grade (Right (Sum x, Sum 100))) = show (roundTo 1 x)-- ++ "%"
  show (Grade (Right (Sum x, Sum y))) = show (roundTo 1 x)-- ++ "/" ++ show y
  show (Grade (Left e)) = show e

instance Ord Grade where
  compare (Grade (Right (Sum x, Sum y))) (Grade (Right (Sum x', Sum y'))) = compare (x/y) (x'/y')
  compare (Grade (Right (Sum _, Sum _))) (Grade (Left _)) = GT
  compare (Grade (Left _)) (Grade (Right (Sum _, Sum _))) = LT
  compare (Grade (Left e)) (Grade (Left e')) = compare e e'

instance Monoid Grade where
  mappend x y = Grade $  (liftA2 (<>)) (unGrade x) (unGrade y)
  mempty = Grade (Right (Sum 0, Sum 0))





gradeError :: GradeErrors -> Grade
gradeError = Grade . Left

unparsed = gradeError Unparsed
absent = gradeError . Absent

unknown :: String -> Grade
unknown = gradeError . Unknown





(.*) :: Grade -> Double -> Grade
x .* t = Grade $ normalize t $ unGrade x
  where normalize z = (fmap (\(Sum a, Sum b) -> (Sum (a/b*z), Sum z)))

x +. y = x <> y


x *.. t = fmap (.*t) x
x *. t = fmap (*..t) x

--f *. t = \u -> f u .* t
--f *.. t = \u v -> f u v .* t

infixl 6 +.
infixl 7 *.



-- Letter Grades
data LetterGrade = O | Ap | A | Am | Bp | B | Bm | Cp | C | Cm | Dp | D | Dm | Ep | E | Em | F | I deriving (Eq, Ord)

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


newtype LGrade = LGrade (Either GradeErrors LetterGrade)



instance Show LGrade where
   show (LGrade (Right g)) = show g
   show (LGrade (Left (Absent _))) = show I
   show (LGrade (Left e)) = show e

instance ByteStringable LGrade

roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)





ifAbsent l f h r = case l h r of 
                      Grade (Left ( Absent n)) -> f n h r
                      y -> y


forAnyAbsent l f h r = map g (l h r)
  where g (Grade (Left ( Absent n))) = f n h r
        g y = y

use l _ h r = l h r
setZero n _ _  = 0 /. n


infixl 8 `ifAbsent`
infixl 8 `forAnyAbsent`


apply f l h r =  f (l h r)

($$) = apply
infixr 0 $$

indexed l = \n -> (!!(n-1)) $$ l

dropGrades n = drop n . sort
combineButDrop n = mconcat . dropGrades n


letterGradeFrom f l h =  LGrade . ((f <$>) . marksEither . l h)
