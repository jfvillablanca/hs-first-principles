data Fiction =
    Fiction
    deriving (Show)

data Nonfiction =
    Nonfiction
    deriving (Show)

data BookType
    = FictionBook Fiction
    | NonfictionBook Nonfiction
    deriving (Show)

type AuthorName = String

data Author =
    Author (AuthorName, BookType)

data AuthorNormalized
    = FictionBook' AuthorName
    | NonfictionBook' AuthorName
    deriving (Eq, Show)
