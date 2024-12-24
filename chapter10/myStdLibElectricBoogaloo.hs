myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x b -> f x || b) False

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- associativity demo
-- usage: demor "+" "0" [1..3]
demor op z xs = foldr (\x y -> concat ["(", x, op, y, ")"]) z (map show xs)

-- usage: demol "+" "0" [1..3]
demol op acc xs = foldl (\x y -> concat ["(", x, op, y, ")"]) acc (map show xs)

myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl f q ls =
    q
        : (case ls of
               [] -> []
               x:xs -> myScanl f (f q x) xs)

fibz :: [Integer]
fibz = 1 : myScanl (+) 1 fibz

factorialz :: [Integer]
factorialz = myScanl (*) 1 [2..]
