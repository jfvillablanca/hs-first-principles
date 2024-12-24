-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE FlexibleInstances #-}

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany (Int, String) where
    tooMany (n, _) = n > 42

-- instance TooMany (Int, Int) where
--     tooMany (n, m) = n + m > 42

instance (Num a, Ord a) => TooMany (a, a) where
    tooMany (x, y) = x + y > 42

newtype Cows = Cows Int deriving (Eq, Show)
newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype GoatInfo = GoatInfo (Int, String) deriving (Eq, Show, TooMany)
newtype GoatCount = GoatCount (Int, Int) deriving (Eq, Show, TooMany)

