data QuantumBool
    = QuantumTrue
    | QuantumFalse
    | QuantumBoth
    deriving (Eq, Show)

data TwoQs =
    MkTwoQs QuantumBool QuantumBool
    deriving (Eq, Show)

type TwoQsTuple = (QuantumBool, QuantumBool)

data Person =
    MkPerson String Int
    deriving (Eq, Show)

jm = MkPerson "julie" 108

ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s

data PersonRecord = PersonRecord
    { name :: String
    , age :: Int
    } deriving (Eq, Show)

data Silly a b c d =
    Silly a b c d
    deriving (Show)
