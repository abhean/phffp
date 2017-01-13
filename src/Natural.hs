module Natural where

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i <  0 = Nothing
  | otherwise = Just . validIntegerToNat $ i
  where validIntegerToNat 0 = Zero
        validIntegerToNat n = Succ . validIntegerToNat $ n - 1
