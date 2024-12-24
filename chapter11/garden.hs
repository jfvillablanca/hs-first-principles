-- data FlowerType
--     = Gardenia
--     | Daisy
--     | Rose
--     | Lilac
--     deriving Show
-- data Garden =
--     Garden Gardener FlowerType
--     deriving Show
type Gardener = String

data GardenSumOfProducts
    = Gardenia Gardener
    | Daisy Gardener
    | Rose Gardener
    | Lilac Gardener
    deriving (Show)
