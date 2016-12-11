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

bindExp :: Integer -> String
bindExp x = let y = 5 in
              let z = y + x in
                "the integer was: " ++ show x ++ " and y was: "
                ++ show y ++ " and z was: " ++ show z

--functionC x y = case x > y of
--  True -> x
--  False -> y

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

myAbs :: (Ord a, Num a) => a -> a
myAbs x
  | x < 0   = -x
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < tooLowBloodNa = "too low"
  | x > tooHighBloodNa = "too high"
  | otherwise = "just right"
  where tooLowBloodNa = 135
        tooHighBloodNa = 145

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

tenDigits :: Integral a => a -> a
tenDigits x = d
  where (xLast, _) = x `divMod` 10
        (_, d) = xLast `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = xLast `mod` 10
  where xLast = x `div` 100

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y

--foldBool3' :: a -> a -> Bool -> a
--foldBool3' x y p = case p of
--  True -> x
--  False ->y

foldBool3'' :: a -> a -> Bool -> a
foldBool3'' x y p
  | p = x
  | otherwise = y

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes t n = 1 + incTimes (t - 1) n

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ = id
applyTimes n f = f . applyTimes (n - 1) f

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacci' :: Integral a => a -> a
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n = fibonacci2 2 1 1
  where fibonacci2 n' pf ppf
          | n == n' = pf + ppf
          | otherwise = fibonacci2 (n' + 1) (pf + ppf) pf

-- WIP:
--fibonacci'' :: Integral a => a -> a
--fibonacci'' 0 = 0
--fibonacci'' 1 = 1
--fibonacci'' n = foldl' fibonacci2 2 (1, 1)
--  where fibonacci2 n' (pf, ppf)
--          | n == n' = pf + ppf
--          | otherwise = fibonacci2 (n' + 1) (pf + ppf, pf)
