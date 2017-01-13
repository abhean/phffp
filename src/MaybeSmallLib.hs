module MaybeSmallLib where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee def _ Nothing = def
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just a) = a

fromJust :: Maybe a -> a
fromJust (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr accMaybeElem (Just [])
  where accMaybeElem Nothing _ = Nothing
        accMaybeElem _ Nothing = Nothing
        accMaybeElem (Just x) (Just xs) = Just (x:xs)
