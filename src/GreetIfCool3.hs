module GreetIfCool3 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True -> putStrLn "eeeyyy. What's shakin'?"
    False -> putStrLn "pssssh."
  where cool = coolness == "downright frosty yo"
