module Lib where

import Mood
import Data.Char
import Data.List
import Data.List.Split

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

dividedBy :: Integral a => a -> a -> Maybe (a, a)
x `dividedBy` 0 = Nothing
x `dividedBy` y = Just (signum (x * y) * result, signum x * quotient)
  where (result, quotient) = go (abs x) (abs y) 0
        go dividend divisor result
          | dividend < divisor = (result, dividend)
          | otherwise = go (dividend - divisor) divisor (result + 1)

sum' :: (Eq a, Num a) => a -> a
sum' n = go n 0
  where go 0 acc = acc
        go n acc = go (n - 1) (acc + n)

product' :: (Integral a) => a -> a -> a
product' x y
    | x < y = go x y 0
    | otherwise = go y x 0
  where go _ 0 _ = 0
        go 0 _ acc = acc
        go x y acc = go (x - 1) y (acc + y)

mc91 :: Integral a => a -> a
mc91 x
    | x > 100 = x - 10
    | otherwise = mc91 . mc91 $ x + 11

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT GT = [GT]
eftOrd GT  _ = []
eftOrd  f  t = reverse . go f t $ []
 where go f t l
        | f == t = f:l
        | t == GT = []
        | otherwise = go (succ f) t (f:l)

myWords :: String -> [String]
myWords [] = []
myWords s = firstWord : myWords rest
  where firstWord = takeWhile (/= ' ') s
        rest = dropWhile (== ' ') . dropWhile (/= ' ') $ s

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
  | p x = x : myFilter p xs
  | otherwise = myFilter p xs

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

filterUpperCase :: String -> String
filterUpperCase = filter isUpper

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter [] = []
capitalizeFirstLetter (x:xs) = toUpper x : xs

--capitalizeWord :: String -> String
--capitalizeWord [] = []
--capitalizeWord (x:xs) = toUpper x : capitalizeWord xs

capitalizeWord' :: String -> String
capitalizeWord' = map toUpper

initial :: String -> Maybe Char
initial [] = Nothing
initial (x:xs) = Just . toUpper $ x

initial' :: String -> Char
initial' = toUpper . head

--myOr :: [Bool] -> Bool
--myOr [] = False
--myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny p (x:xs) = p x || myAny p xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = (x == e) || myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (e==)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = xs ++ [x]

--myReverse' :: [a] -> [a]
--myReverse' l = go l []
--  where go [] r = r
--        go (x:xs) r = go xs (x:r)

--squish :: [[a]] -> [a]
--squish [] = []
--squish (x:xs) = x ++ squish xs

--squishMap :: (a -> [b]) -> [a] -> [b]
--squishMap f = squish . map f

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy comp (x:xs)
  | comp x maximumXs == GT = x
  | otherwise = maximumXs
  where maximumXs = myMaximumBy comp xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy comp (x:xs)
  | comp x minimumXs == LT = x
  | otherwise = minimumXs
  where minimumXs = myMinimumBy comp xs

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myScanl:: (b -> a -> b) -> b -> [a] -> [b]
myScanl f acc l = acc : case l of
    [] -> []
    (x:xs) -> myScanl f (f acc x) xs

wordsAvgLength x = fromIntegral (sumWordsLengths x) / fromIntegral (numWords x)
  where sumWordsLengths = sum . map length . words
        numWords = length . words

--myAndFold :: [Bool] -> Bool
--myAndFold = foldr (&&) True
--
--myOrFold :: [Bool] -> Bool
--myOrFold = foldr (||) False

myAnyFold :: (a -> Bool) -> [a] -> Bool
myAnyFold f = foldr ((||) . f) False

myElemFold :: Eq a => a -> [a] -> Bool
myElemFold elem = foldr ((||) . (== elem)) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny elem = myAnyFold (== elem)

myReverseFold :: [a] -> [a]
myReverseFold = foldr (flip (++) . (:[])) []

myMapFold :: (a -> b) -> [a] -> [b]
myMapFold f = foldr ((:) . f) []

myFilterFold :: (a -> Bool) -> [a] -> [a]
myFilterFold p = foldr (\x acc -> if p x then x:acc else acc) []

--mySquishFold :: [[a]] -> [a]
--mySquishFold = foldr (++) []

mySquishMapFold :: (a -> [b]) -> [a] -> [b]
mySquishMapFold f = foldr ((++) . f) []

mySquishFold' :: [[a]] -> [a]
mySquishFold' = mySquishMapFold id

myMaximumByFold :: (a -> a -> Ordering) -> [a] -> a
myMaximumByFold f [x] = x
myMaximumByFold f (x:xs) = foldr (\x y -> if f x y == GT then x else y) x xs

myMinimumByFold :: (a -> a -> Ordering) -> [a] -> a
myMinimumByFold f [x] = x
myMinimumByFold f (x:xs) = foldr (\x y -> if f x y == LT then x else y) x xs

isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' []  _ = True
isSubsequenceOf'  _ [] = False
isSubsequenceOf' subsequence@(x:xs) (y:ys) = ((x == y) && isSubsequenceOf' xs ys) || isSubsequenceOf' subsequence ys

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\word -> (word, capitalizeWord word)) . words

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate "." . map capitalizeFirstWord . splitOn "."
  where capitalizeFirstWord "" = ""
        capitalizeFirstWord s@(x:xs)
          | isAsciiUpper x = s
          | isAsciiLower x = toUpper x : xs
          | otherwise = x : capitalizeFirstWord xs
