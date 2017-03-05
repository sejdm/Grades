{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Grade
import CassavaBackendMap
import CSVextrasMap
import Safe
import ConfigParser


quizzes = "Quiz" `eachOutOf` 5
quiz = indexed quizzes
quizavg = combine . take 4 . forAnyAbsent quizzes setZero
quizNo = length . quizzes

midterm = "Midterm" `outOf` 30

final = "Final" `outOf` 30

total = midterm  *. 70 +.  quizavg *. 30

letterGrade = letterGradeFrom f total
  where f x | x >= 96 = O
            | x >= 85 = A
            | x >= 75 = B
            | x >= 59 = C
            | x >= 36 = D
            | otherwise = F


  
newCSV = usingList [
    "Id" .= fromColumn "Roll No."
  , "Name" .= fromColumn "Name"
  , "Midterm" .= midterm
  --, "Final" .= final
  , "Quiz 1" .= quiz 1
  , "Quiz 2" .= quiz 2
  , "Quiz 3" .= quiz 3
  , "Quiz 4" .= quiz 4
  , "Quiz 5" .= quiz 5
  , "Quiz Avg" .= quizavg
  , "Total" .= total
  --, "Letter grade" .= letterGrade
                  ]


--main = simple newCSV total
main = simpleMain
