data BinaryTree a
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show, Foldable)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b > a = Node left a (insert' b right)
    | otherwise = Node (insert' b left) a right

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
    if mapTree (+ 1) testTree == mapExpected
        then print "hooray"
        else error "failed"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder (Node Leaf a Leaf) = [a]
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree' == [2, 1, 3]
        then print "hooray"
        else error "failed"

testInorder :: IO ()
testInorder =
    if inorder testTree' == [1, 2, 3]
        then print "hooray"
        else error "failed"

testPostorder :: IO ()
testPostorder =
    if postorder testTree' == [1, 3, 2]
        then print "hooray"
        else error "failed"

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree = foldr
