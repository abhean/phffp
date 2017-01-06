module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' newValue Leaf = Node Leaf newValue Leaf
insert' newValue (Node leftTree nodeValue rightTree)
  | newValue == nodeValue = Node leftTree nodeValue rightTree
  | newValue < nodeValue  = Node (insert' newValue leftTree) nodeValue rightTree
  | newValue > nodeValue  = Node leftTree nodeValue (insert' newValue rightTree)




map' :: (Ord a, Ord b) => (a -> b) -> BinaryTree a -> BinaryTree b
map' _ Leaf = Leaf
map' f (Node left value right) = Node (map' f left) (f value) (map' f right)

mapSorted' :: (Ord a, Ord b) => (a -> b) -> BinaryTree a -> BinaryTree b
mapSorted' _ Leaf = Leaf
mapSorted' f (Node left value right) = Node (mapSorted' f left) (f value) (mapSorted' f right)
