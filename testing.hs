{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction, FlexibleInstances, TemplateHaskell, OverloadedStrings, RankNTypes, FlexibleContexts, DeriveAnyClass #-}
module Main (main) where

import Data.Csv
import Grade
import Control.Lens
import Control.DeepSeq
import GHC.Generics hiding (to, from)



-- The strings that shall be parsed as absent. The rest will throw an error.


-- The available data for each student
data Student = Student {
    _name :: String
  , _sid :: Int
  , _quizzes :: [Grade]
  , _midterm :: Grade
  , _final :: Grade
  } deriving (Show, Generic, NFData)

def = Student {}
makeLenses ''Student




-- The settings:

-- Retrieving information from the CSV files
numOfQuizzes = 5

parseRow = rowParseFor def $ do
      column ["name", "Name"] name
      column ["Roll No", "Reg. No."] sid
      gColumns numOfQuizzes ["q", "Quiz "] quizzes 10
      gColumn ["midterm", "Midterm"] midterm 60
      gColumn ["final", "Final"] final 60

  where gColumn = gradeColumn isAbsent
        gColumns = gradeColumns isAbsent
        isAbsent = ifWords ["ab", "absent", "a", "abs", ""]


-- Grading scheme
quizavg = combineButDrop 1 $$ quizzes /// setZero

total =  midterm // use' final *. 30   +.   final *. 30   +.   quizavg *. 40

letter = letterGradeFrom scheme total
  where scheme x | x >= 96 = O
                 | x >= 85 = A
                 | x >= 75 = B
                 | x >= 59 = C
                 | x >= 36 = D
                 | otherwise = F



-- Headers for the new CSV
myheaders = do
    "Id" ..= sid
    "Name" ..= name
    quizzeslike "Quiz " numOfQuizzes quizzes
    "Quiz Average" ..= quizavg
    "Midterm" ..= midterm
    "Final" ..= final
    "Total" ..= total
    "Letter" ..= letter




main = standardMain $ do
  csvOutput sid "output.csv"
  printStats "Total" total
  printStats "Final" final
  printStats "Quizzes" quizavg
  histOutput 10 "histogram.png" total









-- Boilerplate
instance ToNamedRecord Student where toNamedRecord = createrecords myheaders . force
instance DefaultOrdered Student where headerOrder _ = createheaders myheaders
instance FromNamedRecord Student where parseNamedRecord = parseRow
