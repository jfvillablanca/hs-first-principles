import Data.List (sort)
class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer
    defaultNumber :: a

newtype Age =
    Age Integer
    deriving (Eq, Show)

instance Numberish Age where
    fromNumber = Age
    toNumber (Age n) = n
    defaultNumber = Age 69

newtype Year =
    Year Integer
    deriving (Eq, Show)

instance Numberish Year where
    fromNumber = Year
    toNumber (Year n) = n
    defaultNumber = Year 1969

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a b =
    let summed = toNumber a + toNumber b
     in fromNumber summed

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk _ _ _ = False

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB x a = if x > 0 then aToB a else 2 * aToB a
