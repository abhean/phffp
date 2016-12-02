module Lib where

import Mood

someFunc :: IO ()
someFunc = print $ changeMood Blah

someOtherFunc = let x = 4
                    y = 3
                    z = 5
                in x + y

printDouble :: (Num a, Show a) => a -> IO ()
printDouble n = print doubleN
  where doubleN = 2 * n

printDoubleLet :: (Num a, Show a) => a -> IO ()
printDoubleLet n =
  let doubleN = 2 * n
  in print doubleN

testLetToWhere = x * 3 + y
  where x :: Int
        x = 3
        y = 1000

testLetToWhere2 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

mainPrint2 :: IO()
mainPrint2 = do
  putStrLn "Count to four"
  putStr "one, two"
  putStr ", three, and"
  putStr " four!"

rvrs :: [a] -> [a]
rvrs [] = []
rvrs s = rvrs (drop 1 s) ++ take 1 s

--x = (+)
--
--f xs = w `x` 1
--  where w = length xs

--breakId :: a -> a
--breakId x = do
--  x
--  x

myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, yToZ . xToY $ x)

--data Woot
--data Blah
--
--f :: Woot -> Blah
--f = undefined
--
--g :: (Blah, Woot) -> (Blah, Blah)
--g (x, y) = (x, f y)

--f :: Int -> String
--f = undefined
--
--g:: String -> Char
--g = undefined
--
--h :: Int -> Char
--h = g . f

data X
data Y
data Z

q :: X -> Z
q = undefined

w :: Y -> Z
w = undefined

e :: (X, Y) -> (Z, Z)
e (x, y) = (q x, w y)
