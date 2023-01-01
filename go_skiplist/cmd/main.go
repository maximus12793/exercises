package main

import (
	"fmt"
	skiplist "skiplist_lib"
)

func main() {
	list := skiplist.NewSkipList()

	// Insert some key-value pairs into the skip list.
	list.Insert(1, "a")
	list.Insert(2, "b")
	list.Insert(3, "c")
	list.Insert(4, "d")
	list.Insert(5, "e")

	// Find and print the value for a key.
	key := 3
	value := list.Find(key)
	fmt.Printf("The value for key %d is %s\n", key, value)

	// Delete a key-value pair from the skip list.
	list.Delete(2)

	// Try to find a deleted key.
	value = list.Find(2)
	if value == nil {
		fmt.Println("Key 2 not found")
	} else {
		fmt.Printf("Unexpected value for key 2: %s\n", value)
	}
}
