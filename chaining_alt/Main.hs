module Main where

import Data.Array
import Data.Maybe

import HashTable
import LinkedList hiding (insert)
import Prelude hiding (reverse)


-- Define a simple linked list
main :: IO ()
main = do
  let myList =
        Node (1 :: Integer) (Node (2 :: Integer) (Node (3 :: Integer) Empty))
  -- Print
  printList myList

  -- Insert single element
  let newList = insertSorted (4 :: Integer) myList
  -- Print
  printList newList

-- -- let head = Node 420 Empty
-- -- let list = Node 3 (Node 2 (Node 1 (head)))