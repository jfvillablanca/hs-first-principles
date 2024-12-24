module Cipher where

import Data.Char (isLower, isUpper, toLower, toUpper)

alphabetLen = 26

baseChar = fromEnum 'a'

shift :: Int -> Char -> Char
shift s c
    | isUpper c = normalize c s toUpper toLower
    | isLower c = normalize c s id id
    | otherwise = c
  where
    normalize c' s' tu tl =
        tu . toEnum
            $ (s + fromEnum (tl c) - baseChar) `mod` alphabetLen + baseChar :: Char

caesar :: String -> Int -> String
caesar xs s = map (shift s) xs
