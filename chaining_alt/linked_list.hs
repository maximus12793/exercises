-- Define a function to insert an element into a sorted linked list
insertSorted :: (Ord a) => a -> LinkedList a -> LinkedList a
insertSorted x Empty = Node x Empty
insertSorted x (Node y ys)
  | x <= y    = Node x (Node y ys)
  | otherwise = Node y (insertSorted x ys)