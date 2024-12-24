myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish ([]:xs) = squish xs
squish ([x]:xs) = x : squish xs
squish ((x:xs):xs') = x : squish (xs : xs')

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs@(x:_) = go f xs x
  where
    go _ [] max = max
    go f [x] max =
        case f x max of
            GT -> go f [] x
            _ -> go f [] max
    go f (x:y:xs) max =
        case f x y of
            GT -> go f (y : xs) x
            _ -> go f (y : xs) max

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs@(x:_) = go f xs x
  where
    go _ [] max = max
    go f [x] max =
        case f x max of
            LT -> go f [] x
            _ -> go f [] max
    go f (x:y:xs) max =
        case f x y of
            LT -> go f (y : xs) x
            _ -> go f (y : xs) max

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
