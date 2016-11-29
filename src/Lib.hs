module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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
