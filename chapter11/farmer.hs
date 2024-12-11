newtype Name =
    Name String
    deriving (Show)

newtype Acres =
    Acres Int
    deriving (Show)

data FarmerType
    = DairyFarmer
    | WhereFarmer
    | SoybeanFarmer
    deriving (Show)

data Farmer =
    Farmer Name Acres FarmerType
    deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data Farmer' = Farmer'
    { name :: Name
    , acres :: Acres
    , farmerType :: FarmerType
    } deriving (Show)

isDairyFarmer' :: Farmer' -> Bool
isDairyFarmer' farmer =
    case farmerType farmer of
        DairyFarmer -> True
        _ -> False
