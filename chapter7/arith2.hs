module Arith2 where

f = length . filter (== 'a')

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+ 1)

main :: IO ()
main = do
    print (0 :: Int)
    print (add 1 0)
    print (addOne 0)
    print (addOnePF 0)
    print (addOne . addOne $ 0)
    print (addOnePF . addOne $ 0)
    print (addOne . addOnePF $ 0)
    print (addOnePF . addOnePF $ 0)
    print (negate $ addOne 0)
    print (negate . addOne $ 0)
    print (addOne . addOne . addOne . negate . addOne $ 0)

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = fst $ divMod x 10
    d = xLast `mod` 10

-- hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where
    xLast = fst $ divMod x 100
    d = xLast `mod` 100

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y t =
    case t of
        True -> x
        False -> y

foldBoolIf :: a -> a -> Bool -> a
foldBoolIf x y t =
    if t
        then x
        else y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
