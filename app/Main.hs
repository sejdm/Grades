{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Grade
import CassavaBackend
import CSVextras

quizzes = "Quiz" `eachOutOf` 10

midterm = "Midterm" `outOf` 60

final = "Final" `outOf` 60

quizavg = combineButDrop 1 $$ quizzes `forAnyAbsent` setZero

total = midterm `ifAbsent` use final *. 30   +.  final *. 30   +.  quizavg *. 40

letterGrade = letterGradeFrom f total
  where f x | x >= 96 = O
            | x >= 85 = A
            | x >= 75 = B
            | x >= 59 = C
            | x >= 36 = D
            | otherwise = F


  
newCSV = usingList [
    "Name" .= fromColumn "Name"
  , "Id" .= fromColumn "Reg. No."
  , "Midterm" .= midterm
  , "Final" .= final
  , "Quiz Avg" .= quizavg
  , "Total" .= total
  , "Grade" .= letterGrade
                  ]


main = simple newCSV total
