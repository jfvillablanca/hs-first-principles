eftBool :: Bool -> Bool -> [Bool]
eftBool start stop
    | fromEnum start > fromEnum stop = []
    | start == stop = [stop]
    | otherwise = start : eftBool (succ start) stop

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start stop
    | fromEnum start > fromEnum stop = []
    | start == stop = [stop]
    | otherwise = start : eftOrd (succ start) stop

eftInt :: Int -> Int -> [Int]
eftInt start stop
    | fromEnum start > fromEnum stop = []
    | start == stop = [stop]
    | otherwise = start : eftInt (succ start) stop

eftChar :: Char -> Char -> [Char]
eftChar start stop
    | fromEnum start > fromEnum stop = []
    | start == stop = [stop]
    | otherwise = start : eftChar (succ start) stop
