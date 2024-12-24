zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys = go xs ys []
  where
    go [] _ acc = acc
    go _ [] acc = acc
    go (x:xs') (y:ys') acc = go xs' ys' (acc ++ [(x, y)])

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = go f xs ys []
  where
    go _ [] _ acc = acc
    go _ _ [] acc = acc
    go f (x:xs') (y:ys') acc = go f xs' ys' (acc ++ [f x y])

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)
