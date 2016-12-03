module DayOfWeek where

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

instance Eq DayOfWeek where
  Monday == Monday = True
  Tuesday == Tuesday = True
  Wednesday == Wednesday = True
  Thursday == Thursday = True
  Friday == Friday = True
  Saturday == Saturday = True
  Sunday == Sunday = True
  _ == _ = False

data Date = Date DayOfWeek Int

instance Eq Date where
  (==) (Date dayOfWeek month) (Date dayOfWeek' month') = (dayOfWeek == dayOfWeek) && (month == month')
