module Mood (Mood(..), changeMood) where

data Mood = Blah | Woot deriving Eq

instance Show Mood where
  show Blah = "Mood Blah"
  show Woot = "Mood Woot"

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

settleDown x = if x == Woot
                then Blah
                else x
