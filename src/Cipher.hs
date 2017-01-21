module Cipher where

import Data.Char
import Data.List

letterIndex :: Char -> Int
letterIndex = (+ (-(ord 'a'))) . ord . toLower

offsettedChar :: Int -> Char -> Char
offsettedChar offset c =
      toOriginalCharCase . chr . (+ firstLetterOrd) . (`mod` numLetters) . (+ offset) . (+ (-firstLetterOrd)) . ord . toLower $ c
      where toOriginalCharCase = if isUpper c then toUpper else id
            firstLetterOrd = ord 'a'
            numLetters = (ord 'z' - firstLetterOrd) + 1

cipherCaesar :: String -> Int -> String
cipherCaesar s offset = map (offsettedChar offset) s

unCipherCaesar :: String -> Int -> String
unCipherCaesar s offset = cipherCaesar s (-offset)

cipherVignere :: String -> String -> String
cipherVignere s password = result where
  (result, pass) = foldl' cipherCharWithNextPassChar ("", cycle password) s
   where cipherCharWithNextPassChar (result, pass) ' ' = (result ++ " ", pass)
         cipherCharWithNextPassChar (result, pass) c   = (result ++ [offsettedChar (letterIndex . head $ pass) c], tail pass)

unCipherVignere :: String -> String -> String
unCipherVignere s password = result where
  (result, pass) = foldl' unCipherCharWithNextPassChar ("", cycle password) s
   where unCipherCharWithNextPassChar (result, pass) ' ' = (result ++ " ", pass)
         unCipherCharWithNextPassChar (result, pass) c = (result ++ [offsettedChar (-(letterIndex . head $ pass)) c], tail pass)

cipherUserInput :: (String -> String) -> IO()
cipherUserInput cipherFunc = do
  putStrLn "Type a message:"
  userInput <- getLine
  putStrLn $ "Ciphered message: " ++ cipherFunc userInput

cipherCaesarUserInput :: IO()
cipherCaesarUserInput = cipherUserInput (`cipherCaesar` 1)

cipherVignereUserInput :: IO()
cipherVignereUserInput = cipherUserInput (`cipherVignere` "password")
