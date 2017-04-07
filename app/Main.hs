{-# LANGUAGE OverloadedStrings, Strict #-}
module Main (main) where

import GradesEasy

mth100 = usingMonadStat $ do 
  name <- fromColumn "Name"
  id <- fromColumn "Roll No."
  midterm <- "Midterm" `outOf` 30
  quizzes <- "Quiz" `eachOutOf` 5

  let quizavg = combineButDrop 1 (quizzes `forAnyAbsent` setZero)

  let total = quizavg *. 30 +. (midterm `ifAbsent` setZero) *. 70
  
  let letter = letterGradeFrom f total
                 where f x | x >= 96 = O
                           | x >= 85 = A
                           | x >= 75 = B
                           | x >= 59 = C
                           | x >= 36 = D
                           | otherwise = F

  outputStat total [
      "Id. " .= id
    , "Name" .= name
    , "Total" .= total
    , "Midterm" .= midterm
    , "Quizzes" .= quizavg
    ]

    
--main = simpleMain
main = simpleStat mth100
