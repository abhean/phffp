module Palindrome where

import Data.Char;
import Control.Monad
import System.Exit

palindrome :: IO()
palindrome = forever $ do
  line1 <-  getLine
  let normalizedLine = normalizeString line1 in
    if normalizedLine == reverse normalizedLine
      then putStrLn "It's a palindrome!"
      else do putStrLn "Nope!"
              exitSuccess
  where normalizeString = filter (`elem` ['a'..'z']) . map toLower
