module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

sampleTree = insert' 9 $ insert' 3 $ insert' 5 Leaf

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' newValue Leaf = Node Leaf newValue Leaf
insert' newValue (Node leftTree nodeValue rightTree)
  | newValue == nodeValue = Node leftTree nodeValue rightTree
  | newValue < nodeValue  = Node (insert' newValue leftTree) nodeValue rightTree
  | newValue > nodeValue  = Node leftTree nodeValue (insert' newValue rightTree)

map' :: (Ord a, Ord b) => (a -> b) -> BinaryTree a -> BinaryTree b
map' _ Leaf = Leaf
map' f (Node left value right) = Node (map' f left) (f value) (map' f right)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node leftTree nodeValue rightTree) = foldTree f (f nodeValue (foldTree f acc leftTree)) rightTree

mapSorted' :: (Ord a, Ord b) => (a -> b) -> BinaryTree a -> BinaryTree b
mapSorted' _ Leaf = Leaf
mapSorted' f (Node left value right) = Node (mapSorted' f left) (f value) (mapSorted' f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node leftTree value rightTree) = value : preorder leftTree ++ preorder rightTree

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node leftTree value rightTree) = inorder leftTree ++ value : inorder rightTree

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node leftTree value rightTree) = postorder leftTree ++ postorder rightTree ++ [value]

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
                Nothing -> Leaf
                Just (la, b, ra) -> Node (unfold f la) b (unfold f ra)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x < n then Just (x + 1, x, x + 1) else Nothing) 0
