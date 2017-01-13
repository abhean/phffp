module Words where


newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s
  | length s <= ((*2) . length . filter (`elem` vowels) $ s) = Just . Word' $ s
  | otherwise = Nothing

mkWord' :: String -> Maybe Word'
mkWord' s
  | numConsonants <= numVowels = Just . Word' $ s
  | otherwise = Nothing
  where (numConsonants, numVowels) = foldr incLetterTypeCount (0, 0) s
        incLetterTypeCount letter (numConsontants, numVowels) =
          if letter `elem` vowels then (numConsontants, numVowels + 1) else (numConsontants + 1, numVowels)
