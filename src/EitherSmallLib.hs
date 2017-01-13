module EitherSmallLib where

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> case x of Left a -> a:acc; _ -> acc) []

rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> case x of Right b -> b:acc; _ -> acc) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (\e (as, bs) -> case e of Left a -> (a:as, bs); Right b -> (as, b:bs)) ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just . f $ b
