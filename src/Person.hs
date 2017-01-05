module Person where

--data Person = Person Bool deriving Show
--
--printPerson :: Person -> IO()
--printPerson person = putStrLn (show person)

data Person = Person
  { name :: String
  , age  :: Int }
  deriving (Eq, Show)
