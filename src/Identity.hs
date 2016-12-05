module Identity where

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity x) (Identity x') = x == x'

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn v) (TisAn v') = v == v'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two v1 v2) (Two v1' v2') = (v1 == v1') && (v2 == v2')

data StringOrInt = TisAnInt Int | TisAnString String

instance Eq StringOrInt where
  (==) (TisAnInt v) (TisAnInt v') = v == v'
  (==) (TisAnString v) (TisAnString v') = v == v'
  (==) _ _ = False

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'
