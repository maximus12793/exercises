-- Helpers etc. for HashTable impl.
module HashTable where 

import Data.Array
import Data.Maybe
import LinkedList

import Prelude hiding (reverse)

data HashTable k v = HashTable {
  size :: Int,
  table :: Array Int (LinkedList (k, v))
} 

-- Hash function that maps keys to integers in the range [0, size - 1]
hash :: (k -> Int) -> HashTable k v -> k -> Int
hash h (HashTable s _)  k = h k `mod` s

-- Insert a key-value pair into the hash table
insert :: (Eq k) => (k -> Int) -> k -> v -> HashTable k v -> HashTable k v
insert h k v (HashTable s t) = HashTable s (t // [(h k, Node (k, v) (t ! h k))])


searchList :: (Eq k) => k -> LinkedList (k,v) -> Maybe v
searchList _ Empty = Nothing
searchList k (Node (k', v) xs) | k == k' = Just v | otherwise = searchList k xs

-- Search for a value by key in the hash table
search :: (Eq k) => (k -> Int) -> k -> HashTable k v -> Maybe v
search h k (HashTable s t) = searchList k (t ! h k)

-- Debug: Print out HT contents
printHashTable :: (Show k, Show v) => HashTable k v -> IO ()
printHashTable (HashTable s t) = do
  putStrLn $ "Size: " ++ show s
  putStrLn "Contents:"
  let indices = [0..s-1]
  mapM_ (\i -> putStrLn $ show i ++ ": " ++ show (t ! i)) indices


-- -- Delete a key-value pair from the hash table
-- delete :: (Eq k) => (k -> Int) -> k -> HashTable k v -> HashTable k v
-- delete h k (HashTable s t) = HashTable s (t // [(h k, deleteFromList (k, v) (t ! h k))])

-- -- Helper function that deletes a key-value pair from a linked list
-- deleteFromList :: (Eq k) => (k, v) -> LinkedList (k, v) -> LinkedList (k, v)
-- deleteFromList _ Empty = Empty 
-- deleteFromList p (Node q qs) | p == q    = qs
--                              | otherwise = Node q (deleteFromList p qs)

{-
USAGE GUIDE

-- Create a new hash table with a size of 10
let ht = HashTable 10 (array (0, 9) [(i, Empty) | i <- [0..9]]) :: HashTable Int Int

-- Insert some key-value pairs into the hash table
let ht' = insert h 12 3 ht
let ht'' = insert h 12 55 ht'
let ht''' = insert h 12 15 ht''

printHashTable ht''

-- Working

-- Search for a value by key in the hash table
search (hash h) "hello" ht''  -- Returns Just 42
search (hash h) "world" ht''  -- Returns Just 17
search (hash h) "foo" ht''    -- Returns Nothing

-- Delete a key-value pair from the hash table
ht''' = delete (hash h) "hello" ht''
search (hash h) "hello" ht'''  -- Returns Nothing

-}