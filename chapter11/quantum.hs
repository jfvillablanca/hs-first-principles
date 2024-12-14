data Quantum
    = Yes
    | No
    | Both
    deriving (Eq, Show)

-- 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 = Right No

quantSum3 = Right Both

quantSum4 = Left Yes

quantSum5 = Left No

quantSum6 = Left Both

-- 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 = (Yes, No)

quantProd3 = (Yes, Both)

quantProd4 = (No, Yes)

quantProd5 = (No, No)

quantProd6 = (No, Both)

quantProd7 = (Both, Yes)

quantProd8 = (Both, No)

quantProd9 = (Both, Both)

-- 3 ^ 3
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes = Yes
quantFlip1 No = Yes
quantFlip1 Both = Yes

quantFlip2 Yes = Yes
quantFlip2 No = Yes
quantFlip2 Both = No

quantFlip3 Yes = Yes
quantFlip3 No = Yes
quantFlip3 Both = Both

quantFlip4 Yes = Yes
quantFlip4 No = No
quantFlip4 Both = Yes
-- and 23 more possibilities to go
