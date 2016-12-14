module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = (`mySplit` '\n')

mySplit :: String -> Char -> [String]
mySplit [] _ = []
mySplit sentence separator = takeWhile (/=separator) sentence : ((`mySplit` separator) . dropWhile (==separator) . dropWhile (/=separator) $ sentence)
