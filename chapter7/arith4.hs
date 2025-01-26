module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
    print (roundTrip 4 :: Integer)
    print (id 4)

data SumOfThree a b c
    = FirstPossible a
    | SecondPossible b
    | ThirdPossible c
    deriving (Eq, Show)

sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _) = 0
sumToInt (SecondPossible _) = 1
sumToInt (ThirdPossible _) = 2

f = f

dontDoThis :: Bool -> Int
dontDoThis True = 1
dontDoThis False = error "ops"
