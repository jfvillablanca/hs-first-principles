import Data.Char (isUpper, toUpper)
import Data.List (intersperse)

disgusting = "HbEfLrLxO"

filterUpper = filter isUpper

capitalize :: String -> String
capitalize = concatMap (\(x:xs') -> toUpper x:xs') . intersperse " " .  words

uppercase :: String -> String
uppercase [] = []
uppercase (x:xs) = toUpper x: uppercase xs
