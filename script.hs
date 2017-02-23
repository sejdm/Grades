#!/usr/bin/env stack
{- stack --resolver lts-8.0 --install-ghc
    runghc
-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Grade
import CassavaBackend
import CSVextras

quizzes = "Quiz" `eachOutOf` 10 -- retrieve from columns beginning with "Quiz". Each is out of 10

midterm = "Midterm" `outOf` 60 -- retrieve from column "Midterm" and set the grade out of 60

final = "Final" `outOf` 60

quizavg = combineButDrop 0 $$ quizzes `forAnyAbsent` setZero

total = midterm `ifAbsent` use final *. 30 +.  final *. 30  +.  midterm *. 40

letterGrade = letterGradeFrom f total
  where f x | x >= 96 = O
            | x >= 85 = A
            | x >= 75 = B
            | x >= 59 = C
            | x >= 36 = D
            | otherwise = F


  
newCSV = usingList [
    "name" .= fromColumn "Name"
  , "Id" .= fromColumn "Reg. No."
  , "midterm" .= midterm
  , "final" .= final
  , "Quiz Avg" .= quizavg
  , "total" .= total
  , "Grade" .= letterGrade
                  ]


main = simple newCSV total
