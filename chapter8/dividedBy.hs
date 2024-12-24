type Numerator = Integer

type Denominator = Integer

type Quotient = Integer

type Remainder = Integer

data DividedResult
    = Result Integer
    | DividedByZero
    deriving (Show)

dividedBy :: Numerator -> Denominator -> (Quotient, Remainder)
dividedBy n d = go n d 0
  where
    go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)

sumorial :: (Eq a, Num a) => a -> a
sumorial 0 = 0
sumorial n = n + sumorial (n - 1)

multipliedBy :: (Integral a) => a -> a -> a
multipliedBy x y = go x y 0
  where
    go x' y' acc
        | x' == 0 = acc
        | otherwise = go (x' - 1) y' (acc + y')

dividedByTotalFunction :: Numerator -> Denominator -> DividedResult
dividedByTotalFunction _ 0 = DividedByZero
dividedByTotalFunction n d = Result (div n d)
