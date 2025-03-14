module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines xs = go xs []
  where
    go "" acc = acc
    go xs' acc =
        let x = takeWhile (/= '\n') xs'
            rest =
                case dropWhile (/= '\n') xs' of
                    "" -> ""
                    (_:rest') -> rest'
         in go rest (acc ++ [x])

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
