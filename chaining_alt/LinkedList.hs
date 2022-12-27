-- Helpers etc. for LinkedList impl.
module LinkedList where 

import Prelude hiding (reverse)

data LinkedList a = Empty | Node a (LinkedList a) deriving (Eq, Read, Show)

-- Define a function to insert an element into a sorted linked list
insertSorted :: (Ord a) => a -> LinkedList a -> LinkedList a
insertSorted x Empty = Node x Empty
insertSorted x (Node y ys) | x <= y    = Node x (Node y ys)
                           | otherwise = Node y (insertSorted x ys)

-- Define a function to print the elements of a linked list
printList :: (Show a) => LinkedList a -> IO ()
printList Empty       = putStrLn "End of list"
printList (Node x xs) = do
  print (show x)
  printList xs

-- Insert a single element into LinkedList
insert :: a -> LinkedList a -> LinkedList a
insert = Node

-- Append a single element to LinkedList
append :: LinkedList a -> LinkedList a -> LinkedList a
append Empty ys = ys
append (Node x xs) ys = Node x (xs `append` ys)

-- Reverse the LinkedList
reverse :: LinkedList a -> LinkedList a
reverse Empty = Empty
reverse (Node x xs) = reverse xs `append` Node x Empty
