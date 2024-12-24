module Vigenere where

import Data.Char (chr, isAlpha, isLower, ord, toLower, toUpper)

shift :: (Int, Char) -> Char
shift (n, c) =
    if isAlpha c
        then if isLower c
                 then go c
                 else toUpper $ go (toLower c)
        else c
  where
    go c' = chr ((ord c' - ord 'a' + n) `mod` 26 + ord 'a')

arst :: String -> String -> [Int]
arst [] _ = []
arst (x:xs) keyword =
    let zxcv =
            reverse
                $ foldl
                      (\acc@((i, _, _):_) x' ->
                           if isAlpha x'
                               then (i + 1, go (i + 1), x') : acc
                               else (i, go i, x') : acc)
                      [(0, go 0, x)]
                      xs
          where
            go j =
                let key = cycle keyword !! j
                    shiftAmt = ord (toLower key) - ord 'a'
                 in shiftAmt
     in [i | (_, i, _) <- zxcv]

myMsg = "MEET at Dawn"
myKey = "ALLY"

vigenere :: String -> String -> String
vigenere keyword msg =
    -- let qwfp = arst msg keyword
    --     zipped = zip qwfp msg
    --     res = map shift zipped
    --  in res
    -- let qwfp = arst msg keyword
    --     zipped = zip qwfp msg
    --     res = map shift zipped
    --  in
     zipWith (curry shift) (arst msg keyword) msg
