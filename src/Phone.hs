module Phone where

import Data.Char
import Data.List

data DaPhone = DaPhone [(Digit, [Char])]

type Digit = Char
type Presses = Int

getCharDigitPresses :: Char -> [(Digit, [Char])] -> (Digit, Presses)
getCharDigitPresses c ((digit, chars) : digitsChars) =
  case charElemIndex of
    Nothing -> getCharDigitPresses c digitsChars
    Just charIndex -> (digit, charIndex + 1)
  where charElemIndex = elemIndex c chars

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone digitsChars) c
  | isAsciiUpper c = [getCharDigitPresses '^' digitsChars, getCharDigitPresses (toLower c) digitsChars]
  | otherwise = [getCharDigitPresses c digitsChars]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead daPhone = concatMap (reverseTaps daPhone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . snd . unzip

charFingerTaps :: DaPhone -> Char -> Presses
charFingerTaps daPhone = fingerTaps . reverseTaps daPhone

mostPopularLetter :: String -> Char
mostPopularLetter = head . minimumBy (\s1 s2 -> compare (length s2) (length s1)) . group . sort

defaultPhone = DaPhone
  [
  ('1', ""),
  ('2', "abc2"),
  ('3', "def3"),
  ('4', "ghi4"),
  ('5', "jkl5"),
  ('6', "mno6"),
  ('7', "pqrs7"),
  ('8', "tuv8"),
  ('9', "wxyz9"),
  ('*', "*^"),
  ('0', "+_ 0"),
  ('#', "#.,")
  ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"]
