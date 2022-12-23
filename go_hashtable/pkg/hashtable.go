package hashtable_lib

import (
	"container/list"
	"fmt"
	"hash/fnv"
)

type HashTableInterface interface {
	init()
	get()
	put()
	remove()
	resize()
	hash()
}

type KeyValuePair struct {
	Key   string
	Value int
}

type HashTable struct {
	Buckets []*list.List
	size    int
}

const keyNotFoundError = "Key not found - %s"

/*
PUBLIC METHODS
*/
func (h *HashTable) Init(numBuckets int) {
	// Make N default linked lists.
	h.size = numBuckets
	h.Buckets = make([]*list.List, numBuckets)
	for i := 0; i < numBuckets; i++ {
		// Initialize each LL.
		h.Buckets[i] = list.New()
	}
}

func (h *HashTable) Get(key string) (int, error) {
	bucket := h.findBucket(key)

	for e := bucket.Front(); e != nil; e = e.Next() {
		pair := e.Value.(KeyValuePair)
		if pair.Key == key {
			return pair.Value, nil
		}
	}
	return 0, fmt.Errorf(keyNotFoundError, key)
}

func (h *HashTable) Put(kvp KeyValuePair) {
	bucket := h.findBucket(kvp.Key)
	bucket.PushBack(kvp)
}

func (h *HashTable) Remove(key string) error {
	bucket := h.findBucket(key)
	if bucket.Len() == 0 {
		return fmt.Errorf(keyNotFoundError, key)
	}
	for e := bucket.Front(); e != nil; e = e.Next() {
		pair := e.Value.(KeyValuePair)
		if pair.Key == key {
			bucket.Remove(e)
			return nil
		}
	}
	return fmt.Errorf(keyNotFoundError, key)
}

/*
PRIVATE METHODS
*/
func (h *HashTable) hash(key string) int {
	// Todo: remove hash/fvn dep
	hasher := fnv.New32a()
	hasher.Write([]byte(fmt.Sprintf("%v", key)))
	hash := hasher.Sum32()

	// Mod to find location.
	// msg := fmt.Sprintf("key %s size %d", key, h.size)
	// fmt.Println(msg)
	return int(hash) % h.size
}

/*
HELPER METHODS
*/
func (h *HashTable) findBucket(key string) *list.List {
	// Calculate bucket for a key.
	bucketIndex := h.hash(key)
	bucket := h.Buckets[bucketIndex]
	return bucket
}

func Example(s string) string {
	return s
}

// TODO: Make generic
// TODO: Try diff hashing algos.
