package main

import (
	"fmt"
	htl "hashtable_lib"
)

func main() {
	hashtable := new(htl.HashTable)
	hashtable.Init(8) // Create 8 buckets.

	val, err := hashtable.Get("foo")
	fmt.Println(err)

	kvp := htl.KeyValuePair{Key: "foo", Value: 123}
	hashtable.Put(kvp)
	val, err = hashtable.Get("foo")
	fmt.Println(val)

	err = hashtable.Remove("foo")
	if err != nil {
		fmt.Println("1")
		fmt.Println(err)
	}
	err = hashtable.Remove("foo")
	if err != nil {
		fmt.Println("2")
		fmt.Println(err)
	}

	hashtable.Put(htl.KeyValuePair{Key: 111, Value: -3})
	hashtable.Put(htl.KeyValuePair{Key: "foo", Value: 10})
	hashtable.Put(htl.KeyValuePair{Key: "bar", Value: 20})
	hashtable.Put(htl.KeyValuePair{Key: "baz", Value: 30})
	hashtable.Print()
}
