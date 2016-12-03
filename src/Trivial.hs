module Trivial where

data Trivial = Trivial

instance Eq Trivial where
  _ == _ = True
