{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, yToZ $ xToY x)

i :: a -> a
i = id

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' = c

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r = id

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

a :: (a -> c) -> a -> a
a _ a = a

a' :: (a -> b) -> a -> b
a' aToB = aToB
