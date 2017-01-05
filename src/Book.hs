module Book where

type AuthorName = String

--data Fiction = Fiction deriving Show
--data NonFiction = Nonfiction deriving Show

--data BookType = FictionBook Fiction
--  | NonfictionBook NonFiction
--  deriving Show

--data Author = Author (AuthorName, BookType)

data Author = Fiction AuthorName | Nonfiction AuthorName
  deriving (Show, Eq)
