import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

data DatabaseItem
    = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    , DbNumber 69
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
    foldr
        (\a b ->
             case a of
                 DbDate a' -> a' : b
                 _ -> b)
        []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
    foldr
        (\a b ->
             case a of
                 DbNumber a' -> a' : b
                 _ -> b)
        []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs =
    foldr
        (\a b ->
             case a of
                 DbDate a' -> max a' b
                 _ -> b)
        (head . filterDbDate $ xs)
        xs

sumDb :: [DatabaseItem] -> Integer
sumDb =
    foldr
        (\a b ->
             case a of
                 DbNumber a' -> a' + b
                 _ -> b)
        0

-- avgDb :: [DatabaseItem] -> Double
avgDb xs =
    foldr (\a b -> 
            let
                        go item acc count = undefined
             in
                go a b 1
        )
        (head . filterDbNumber $ xs)
        xs

