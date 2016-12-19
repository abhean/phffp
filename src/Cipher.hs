module Cipher where

import Data.Char

cipherCaesar :: String -> Int -> String
cipherCaesar s offset = map (offsetChar offset) s
  where
      offsetChar :: Int -> Char -> Char
      offsetChar offset c =
            toOriginalCharCase . chr . (+ firstLetterOrd) . (`mod` numLetters) . (+ offset) . (+ (-firstLetterOrd)) . ord . toLower $ c
            where toOriginalCharCase = if isUpper c then toUpper else id
                  firstLetterOrd = ord 'a'
                  numLetters = (ord 'z' - firstLetterOrd) + 1

unCipherCaesar :: String -> Int -> String
unCipherCaesar s offset = cipherCaesar s (-offset)
