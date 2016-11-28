module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

someOtherFunc = let x = 4
                    y = 3
                in x + y
