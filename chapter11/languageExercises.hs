import Data.Char (toUpper)
import Data.List (intersperse)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph xs = concatMap capitalizeWord $ intersperse " " $ words xs
