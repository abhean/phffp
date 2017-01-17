module Unfolds where


myIterate :: (a -> a) -> a -> [a]
myIterate f x = go f x []
  where go f x xs = x : go f (f x) xs

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = go f x []
  where go f b as = case f b of
                      Nothing -> as
                      Just (a, nb) -> a : go f nb as

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))
