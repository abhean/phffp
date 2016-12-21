module Database where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbNumber 13
    , DbDate (UTCTime
      (fromGregorian 1921 5 1)
      (secondsToDiffTime 34123))
  ]

filterDbNumber :: [DatabaseItem] -> [DatabaseItem]
filterDbNumber  = foldr (\item result-> case item of (DbNumber _) -> item:result; _ -> result) []

filterDbDate :: [DatabaseItem] -> [DatabaseItem]
filterDbDate = foldr (\item result-> case item of (DbDate _) -> item:result; _ -> result) []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . extractTimes
  where extractTimes = foldr (\item ts -> case item of (DbDate t) -> t:ts; _ -> ts) []

sumDb :: [DatabaseItem] -> Integer
sumDb  = sum . extractNumbers
    where extractNumbers = foldr (\item ns -> case item of (DbNumber n) -> n:ns; _ -> ns) []

avgDb :: [DatabaseItem] -> Double
avgDb  = average . extractNumbers
    where extractNumbers = foldr (\item ns -> case item of (DbNumber n) -> n:ns; _ -> ns) []
          average l = (fromIntegral . sum $ l) / (fromIntegral . length $ l)
