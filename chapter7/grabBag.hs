mTh x y z = x * y * z

mTh' x y = \z -> x * y * z

mTh'' x = \y -> \z -> x * y * z

-- by default, the inferred type becomes Integer -> Integer -> Integer -> Integer
mTh''' :: Num a => a -> a -> a -> a
mTh''' = \x -> \y -> \z -> x * y * z

addOneIfOdd n =
    case odd n of
        True -> f n
        False -> n
  where
    f = \n -> n + 1

addFive =
    \x ->
        \y ->
            (if x > y
                 then y
                 else x)
                + 5

mflip f x y = f y x

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False
