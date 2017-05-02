{-# LANGUAGE OverloadedStrings, Strict #-}
module Main (main) where

import GradesEasy

mth100 = usingMonadStatSafe $ do 
  name <- fromColumn "Name"
  id <- fromColumn "Roll No."
  midterm <- "Midterm" `outOf` 30
  final <- "Final" `outOf` 30
  quizzes <- "Quiz" `eachOutOf` 5

  let quizavg = combineButDrop 2 (quizzes `forAnyAbsent` setZero)

  --let total = quizavg *. 40 +. (midterm `ifAbsent` setZero) *. 70
  let total = quizavg *. 40 +. midterm *. 30 +. final *. 30

  
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
    , "Quizzes" .= quizavg
    , "Midterm" .= midterm
    , "Final" .= midterm
    ]

    
main = simpleMain
--main = simpleStatSafe mth100
