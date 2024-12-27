myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = not (x == False) && myAnd xs

myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = foldr (&&) x xs

myPointFreeAnd :: [Bool] -> Bool
myPointFreeAnd = foldr (&&) True

myPointFreeOr :: [Bool] -> Bool
myPointFreeOr = foldr (||) False

myAlmostPointFreeAny :: (a -> Bool) -> [a] -> Bool
myAlmostPointFreeAny f = foldr ((||) . f) False

myAlmostPointFreeElem :: Eq a => a -> [a] -> Bool
myAlmostPointFreeElem a = foldr ((||) . (== a)) False

myAlmostPointFreeElem' :: Eq a => a -> [a] -> Bool
myAlmostPointFreeElem' a = myAlmostPointFreeAny (== a)

myPointFreeReverse :: [a] -> [a]
myPointFreeReverse = foldl (flip (:)) []

myAlmostPointFreeMap :: (a -> b) -> [a] -> [b]
myAlmostPointFreeMap f = foldr ((:) . f) []

myAlmostPointFreeFilter :: (a -> Bool) -> [a] -> [a]
myAlmostPointFreeFilter f =
    foldr
        (\x acc ->
             if f x
                 then x : acc
                 else acc)
        []

mySquish :: [[a]] -> [a]
mySquish = foldr (\xs acc -> go xs ++ acc) []
  where
    go [] = []
    go (x:xs) = x : go xs

mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap f = mySquish . map f

mySquishAgain :: [[a]] -> [a]
mySquishAgain = mySquishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f li@(x:xs) =
    foldr
        (\x max ->
             case f x max of
                 GT -> x
                 _ -> max)
        (head li)
        li


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f li@(x:xs) =
    foldr
        (\x min ->
             case f x min of
                 LT -> x
                 _ -> min)
        (head li)
        li
