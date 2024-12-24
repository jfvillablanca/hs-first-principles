module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
    | n == 0 = "zero"
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"

digits :: Int -> [Int]
digits n = go n []
  where
    go :: Int -> [Int] -> [Int]
    go 0 acc = acc
    go n acc =
        let (n', digit) = quotRem n 10
         in go n' (abs digit : acc)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
