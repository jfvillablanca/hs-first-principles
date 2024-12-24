data OperatingSystem
    = GnuPlusLinux
    | OpenBSD
    | Mac
    | Win
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSD, Mac, Win]

data ProgLang
    = Haskell
    | Agda
    | Idris
    | Purescript
    deriving (Eq, Show)

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, Purescript]

data Programmer = Programmer
    { os :: OperatingSystem
    , lang :: ProgLang
    } deriving (Eq, Show)

data Programmer' =
    Programmer' OperatingSystem ProgLang

allProgrammers :: [Programmer]
allProgrammers =
    [ Programmer {lang = l, os = o}
    | l <- allLanguages
    , o <- allOperatingSystems
    ]
