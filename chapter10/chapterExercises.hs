{-# LANGUAGE TupleSections #-}

stops = "pbtdkg"

vowels = "aeiou"

nouns = ["dog", "cat"]

verbs = ["jump", "fly"]

makeCombo :: (a, a) -> a -> [a]
makeCombo (c, c') v = [c, v, c']

getPairs :: [a] -> [(a, a)]
getPairs src = go src src
  where
    go [] _ = []
    go (x:xs) corpus = map (x, ) corpus ++ go xs corpus

permute :: [a] -> [a] -> [[a]]
permute surroundingSrc surroundedSrc =
    concatMap (\pair -> map (makeCombo pair) surroundedSrc)
        $ getPairs surroundingSrc

onlyStartWithP :: [[Char]] -> [[Char]]
onlyStartWithP = filter (\(x:xs) -> x == 'p')

seekritFunc :: String -> Double
seekritFunc x =
    (/) (sum (map (fromIntegral . length) (words x)))
        ((fromIntegral . length) (words x))
