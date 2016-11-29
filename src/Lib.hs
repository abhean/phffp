module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

someOtherFunc = let x = 4
                    y = 3
                    z = 5
                in x + y
